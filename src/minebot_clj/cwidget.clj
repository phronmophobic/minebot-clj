(ns minebot-clj.cwidget

  (:import (com.trolltech.qt.core QObject)
           (com.trolltech.qt.gui QWidget)
           (java.lang.reflect Modifier Constructor)
           (clojure.asm ClassWriter ClassVisitor Opcodes Type )
           (clojure.asm.commons Method GeneratorAdapter )
           (clojure.lang IPersistentMap DynamicClassLoader RT)

           ))


;(defn method-sig [^java.lang.reflect.Method meth]
;  [(. meth (getName)) (seq (. meth (getParameterTypes)))])

(defn- filter-methods [^Class c invalid-method?]
  (loop [mm {}
         considered #{}
         c c]
    (if c
      (let [[mm considered]
            (loop [mm mm
                   considered considered
                   meths (seq (concat
                                (seq (. c (getDeclaredMethods)))
                                (seq (. c (getMethods)))))]
              (if meths
                (let [^java.lang.reflect.Method meth (first meths)
                      mods (. meth (getModifiers))
                      mk (method-sig meth)]
                  (if (or (considered mk)
                          (invalid-method? meth))
                    (recur mm (conj considered mk) (next meths))
                    (recur (assoc mm mk meth) (conj considered mk) (next meths))))
                [mm considered]))]
        (recur mm considered (. c (getSuperclass))))
      mm)))

(defn- non-private-methods [^Class c]
  (let [not-overridable? (fn [^java.lang.reflect.Method meth]
                           (let [mods (. meth (getModifiers))]
                             (or (not (or (Modifier/isPublic mods) (Modifier/isProtected mods)))
                                 (. Modifier (isStatic mods))
                                 (. Modifier (isFinal mods))
                                 (= "finalize" (.getName meth)))))]
    (filter-methods c not-overridable?)))

(defn- protected-final-methods [^Class c]
  (let [not-exposable? (fn [^java.lang.reflect.Method meth]
                         (let [mods (. meth (getModifiers))]
                           (not (and (Modifier/isProtected mods)
                                     (Modifier/isFinal mods)
                                     (not (Modifier/isStatic mods))))))]
    (filter-methods c not-exposable?)))

(defn- ctor-sigs [^Class super]
  (for [^Constructor ctor (. super (getDeclaredConstructors))
        :when (not (. Modifier (isPrivate (. ctor (getModifiers)))))]
    (apply vector (. ctor (getParameterTypes)))))

(defn- escape-class-name [^Class c]
  (.. (.getSimpleName c) 
      (replace "[]" "<>")))

(defn- overload-name [mname pclasses]
  (if (seq pclasses)
    (apply str mname (interleave (repeat \-) 
                                 (map escape-class-name pclasses)))
    (str mname "-void")))

(defn- ^java.lang.reflect.Field find-field [^Class c f]
  (let [start-class c]
    (loop [c c]
      (if (= c Object)
        (throw (new Exception (str "field, " f ", not defined in class, " start-class ", or its ancestors")))
        (let [dflds (.getDeclaredFields c)
              rfld (first (filter #(= f (.getName ^java.lang.reflect.Field %)) dflds))]
          (or rfld (recur (.getSuperclass c))))))))

;(distinct (map first(keys (mapcat non-private-methods [Object IPersistentMap]))))

(def ^{:private true} prim->class
     {'int Integer/TYPE
      'ints (Class/forName "[I")
      'long Long/TYPE
      'longs (Class/forName "[J")
      'float Float/TYPE
      'floats (Class/forName "[F")
      'double Double/TYPE
      'doubles (Class/forName "[D")
      'void Void/TYPE
      'short Short/TYPE
      'shorts (Class/forName "[S")
      'boolean Boolean/TYPE
      'booleans (Class/forName "[Z")
      'byte Byte/TYPE
      'bytes (Class/forName "[B")
      'char Character/TYPE
      'chars (Class/forName "[C")})

(defn- ^Class the-class [x] 
  (cond 
   (class? x) x
   (contains? prim->class x) (prim->class x)
   :else (let [strx (str x)]
           (clojure.lang.RT/classForName 
            (if (some #{\. \[} strx)
              strx
              (str "java.lang." strx))))))

;; someday this can be made codepoint aware
(defn- valid-java-method-name
  [^String s]
  (= s (clojure.lang.Compiler/munge s)))

(defn- validate-generate-class-options
  [{:keys [methods]}]
  (let [[mname] (remove valid-java-method-name (map (comp str first) methods))]
    (when mname (throw (IllegalArgumentException. (str "Not a valid method name: " mname))))))

(def add-annotations (ns-resolve 'clojure.core 'add-annotations))

(defn generate-class [options-map]
  (validate-generate-class-options options-map)
  (let [default-options {:prefix "-" :load-impl-ns true :impl-ns (ns-name *ns*)}
        {:keys [name extends implements constructors methods main factory state init exposes 
                exposes-methods prefix load-impl-ns impl-ns post-init field-names]} 
          (merge default-options options-map)
        name-meta (meta name)
        name (str name)
        super (if extends (the-class extends) Object)
        interfaces (map the-class implements)
        supers (cons super interfaces)
        ctor-sig-map (or constructors (zipmap (ctor-sigs super) (ctor-sigs super)))
        cv (new ClassWriter (. ClassWriter COMPUTE_MAXS))
        cname (. name (replace "." "/"))
        pkg-name name
        impl-pkg-name (str impl-ns)
        impl-cname (.. impl-pkg-name (replace "." "/") (replace \- \_))
        ctype (. Type (getObjectType cname))
        iname (fn [^Class c] (.. Type (getType c) (getInternalName)))
        totype (fn [^Class c] (. Type (getType c)))
        to-types (fn [cs] (if (pos? (count cs))
                            (into-array (map totype cs))
                            (make-array Type 0)))
        obj-type ^Type (totype Object)
        arg-types (fn [n] (if (pos? n)
                            (into-array (replicate n obj-type))
                            (make-array Type 0)))
        super-type ^Type (totype super)
        init-name (str init)
        post-init-name (str post-init)
        factory-name (str factory)
        state-name (str state)
        main-name "main"
        var-name (fn [s] (clojure.lang.Compiler/munge (str s "__var")))
        class-type  (totype Class)
        rt-type  (totype clojure.lang.RT)
        var-type ^Type (totype clojure.lang.Var)
        ifn-type (totype clojure.lang.IFn)
        iseq-type (totype clojure.lang.ISeq)
        ex-type  (totype java.lang.UnsupportedOperationException)
        all-sigs (distinct (concat (map #(let[[m p] (key %)] {m [p]}) (mapcat non-private-methods supers))
                                   (map (fn [[m p]] {(str m) [p]}) methods)))
        sigs-by-name (apply merge-with concat {} all-sigs)
        overloads (into {} (filter (fn [[m s]] (next s)) sigs-by-name))
        var-fields (concat (when init [init-name]) 
                           (when post-init [post-init-name])
                           (when main [main-name])
                           ;(when exposes-methods (map str (vals exposes-methods)))
                           (distinct (concat (keys sigs-by-name)
                                             (mapcat (fn [[m s]] (map #(overload-name m (map the-class %)) s)) overloads)
                                             (mapcat (comp (partial map str) vals val) exposes))))
        emit-get-var (fn [^GeneratorAdapter gen v]
                       (let [false-label (. gen newLabel)
                             end-label (. gen newLabel)]
                         (. gen getStatic ctype (var-name v) var-type)
                         (. gen dup)
                         (. gen invokeVirtual var-type (. Method (getMethod "boolean isBound()")))
                         (. gen ifZCmp (. GeneratorAdapter EQ) false-label)
                         (. gen invokeVirtual var-type (. Method (getMethod "Object get()")))
                         (. gen goTo end-label)
                         (. gen mark false-label)
                         (. gen pop)
                         (. gen visitInsn (. Opcodes ACONST_NULL))
                         (. gen mark end-label)))
        emit-unsupported (fn [^GeneratorAdapter gen ^Method m]
                           (. gen (throwException ex-type (str (. m (getName)) " ("
                                                               impl-pkg-name "/" prefix (.getName m)
                                                               " not defined?)"))))
        emit-forwarding-method
        (fn [name pclasses rclass as-static else-gen]
          (let [mname (str name)
                pmetas (map meta pclasses)
                pclasses (map the-class pclasses)
                rclass (the-class rclass)
                ptypes (to-types pclasses)
                rtype ^Type (totype rclass)
                m (new Method mname rtype ptypes)
                is-overload (seq (overloads mname))
                gen (new GeneratorAdapter (+ (. Opcodes ACC_PUBLIC) (if as-static (. Opcodes ACC_STATIC) 0)) 
                         m nil nil cv)
                found-label (. gen (newLabel))
                else-label (. gen (newLabel))
                end-label (. gen (newLabel))]
            (add-annotations gen (meta name))
            (dotimes [i (count pmetas)]
              (add-annotations gen (nth pmetas i) i))
            (. gen (visitCode))
            (if (> (count pclasses) 18)
              (else-gen gen m)
              (do
                (when is-overload
                  (emit-get-var gen (overload-name mname pclasses))
                  (. gen (dup))
                  (. gen (ifNonNull found-label))
                  (. gen (pop)))
                (emit-get-var gen mname)
                (. gen (dup))
                (. gen (ifNull else-label))
                (when is-overload
                  (. gen (mark found-label)))
                                        ;if found
                (.checkCast gen ifn-type)
                (when-not as-static
                  (. gen (loadThis)))
                                        ;box args
                (dotimes [i (count ptypes)]
                  (. gen (loadArg i))
                  (. clojure.lang.Compiler$HostExpr (emitBoxReturn nil gen (nth pclasses i))))
                                        ;call fn
                (. gen (invokeInterface ifn-type (new Method "invoke" obj-type 
                                                      (to-types (replicate (+ (count ptypes)
                                                                              (if as-static 0 1)) 
                                                                           Object)))))
                                        ;(into-array (cons obj-type 
                                        ;                 (replicate (count ptypes) obj-type))))))
                                        ;unbox return
                (. gen (unbox rtype))
                (when (= (. rtype (getSort)) (. Type VOID))
                  (. gen (pop)))
                (. gen (goTo end-label))
                
                                        ;else call supplied alternative generator
                (. gen (mark else-label))
                (. gen (pop))
                
                (else-gen gen m)
            
                (. gen (mark end-label))))
            (. gen (returnValue))
            (. gen (endMethod))))
        ]
                                        ;start class definition
    (. cv (visit (. Opcodes V1_5) (+ (. Opcodes ACC_PUBLIC) (. Opcodes ACC_SUPER))
                 cname nil (iname super)
                 (when-let [ifc (seq interfaces)]
                   (into-array (map iname ifc)))))

                                        ; class annotations
    (add-annotations cv name-meta)
    
                                        ;static fields for vars
    (doseq [v var-fields]
      (. cv (visitField (+ (. Opcodes ACC_PRIVATE) (. Opcodes ACC_FINAL) (. Opcodes ACC_STATIC))
                        (var-name v) 
                        (. var-type getDescriptor)
                        nil nil)))
    
                                        ;instance field for state
    (when state
      (. cv (visitField (+ (. Opcodes ACC_PUBLIC) (. Opcodes ACC_FINAL))
                        state-name 
                        (. obj-type getDescriptor)
                        nil nil)))

    (doseq [field field-names
            :let [fname (clojure.core/name field)
                  cls (get (meta field) :class Object)]]
      (. cv (visitField (+ (. Opcodes ACC_PUBLIC))
                        fname
                        (. (totype cls) getDescriptor)
                        nil nil)))


    
                                        ;static init to set up var fields and load init
    (let [gen (new GeneratorAdapter (+ (. Opcodes ACC_PUBLIC) (. Opcodes ACC_STATIC)) 
                   (. Method getMethod "void <clinit> ()")
                   nil nil cv)]
      (. gen (visitCode))
      (doseq [v var-fields]
        (. gen push impl-pkg-name)
        (. gen push (str prefix v))
        (. gen (invokeStatic var-type (. Method (getMethod "clojure.lang.Var internPrivate(String,String)"))))
        (. gen putStatic ctype (var-name v) var-type))
      
      (when load-impl-ns
        (. gen push "clojure.core")
        (. gen push "load")
        (. gen (invokeStatic rt-type (. Method (getMethod "clojure.lang.Var var(String,String)"))))
        (. gen push (str "/" impl-cname))
        (. gen (invokeInterface ifn-type (new Method "invoke" obj-type (to-types [Object]))))
;        (. gen push (str (.replace impl-pkg-name \- \_) "__init"))
;        (. gen (invokeStatic class-type (. Method (getMethod "Class forName(String)"))))
        (. gen pop))

      (. gen (returnValue))
      (. gen (endMethod)))
    
                                        ;ctors
    (doseq [[pclasses super-pclasses] ctor-sig-map]
      (let [constructor-annotations (meta pclasses)
            pclasses (map the-class pclasses)
            super-pclasses (map the-class super-pclasses)
            ptypes (to-types pclasses)
            super-ptypes (to-types super-pclasses)
            m (new Method "<init>" (. Type VOID_TYPE) ptypes)
            super-m (new Method "<init>" (. Type VOID_TYPE) super-ptypes)
            gen (new GeneratorAdapter (. Opcodes ACC_PUBLIC) m nil nil cv)
            _ (add-annotations gen constructor-annotations)
            no-init-label (. gen newLabel)
            end-label (. gen newLabel)
            no-post-init-label (. gen newLabel)
            end-post-init-label (. gen newLabel)
            nth-method (. Method (getMethod "Object nth(Object,int)"))
            local (. gen newLocal obj-type)]
        (. gen (visitCode))
        
        (if init
          (do
            (emit-get-var gen init-name)
            (. gen dup)
            (. gen ifNull no-init-label)
            (.checkCast gen ifn-type)
                                        ;box init args
            (dotimes [i (count pclasses)]
              (. gen (loadArg i))
              (. clojure.lang.Compiler$HostExpr (emitBoxReturn nil gen (nth pclasses i))))
                                        ;call init fn
            (. gen (invokeInterface ifn-type (new Method "invoke" obj-type 
                                                  (arg-types (count ptypes)))))
                                        ;expecting [[super-ctor-args] state] returned
            (. gen dup)
            (. gen push (int 0))
            (. gen (invokeStatic rt-type nth-method))
            (. gen storeLocal local)
            
            (. gen (loadThis))
            (. gen dupX1)
            (dotimes [i (count super-pclasses)]
              (. gen loadLocal local)
              (. gen push (int i))
              (. gen (invokeStatic rt-type nth-method))
              (. clojure.lang.Compiler$HostExpr (emitUnboxArg nil gen (nth super-pclasses i))))
            (. gen (invokeConstructor super-type super-m))
            

            ;;;;;

            ;; (. gen dup2)

            ;; (. gen push (int 2))
            ;; (. gen (invokeStatic rt-type nth-method))
            ;; (. gen storeLocal local)
            
            ;; (doseq [[i fname] (map-indexed vector field-names)]
            ;;   (. gen dup)
            ;;   (. gen loadLocal local)
            ;;   (. gen push (int i))
            ;;   (. gen (invokeStatic rt-type nth-method))
            ;;   (. gen (putField ctype fname obj-type)))
            ;; (. gen pop)

            ;;;;;;;
            
            (if state
              (do
                (. gen push (int 1))
                (. gen (invokeStatic rt-type nth-method))
                (. gen (putField ctype state-name obj-type)))
              (. gen pop))
            
            (. gen goTo end-label)
                                        ;no init found
            (. gen mark no-init-label)
            (. gen (throwException ex-type (str impl-pkg-name "/" prefix init-name " not defined")))
            (. gen mark end-label))
          (if (= pclasses super-pclasses)
            (do
              (. gen (loadThis))
              (. gen (loadArgs))
              (. gen (invokeConstructor super-type super-m)))
            (throw (new Exception ":init not specified, but ctor and super ctor args differ"))))

        (when post-init
          (emit-get-var gen post-init-name)
          (. gen dup)
          (. gen ifNull no-post-init-label)
          (.checkCast gen ifn-type)
          (. gen (loadThis))
                                       ;box init args
          (dotimes [i (count pclasses)]
            (. gen (loadArg i))
            (. clojure.lang.Compiler$HostExpr (emitBoxReturn nil gen (nth pclasses i))))
                                       ;call init fn
          (. gen (invokeInterface ifn-type (new Method "invoke" obj-type 
                                                (arg-types (inc (count ptypes))))))
          (. gen pop)
          (. gen goTo end-post-init-label)
                                       ;no init found
          (. gen mark no-post-init-label)
          (. gen (throwException ex-type (str impl-pkg-name "/" prefix post-init-name " not defined")))
          (. gen mark end-post-init-label))

        (. gen (returnValue))
        (. gen (endMethod))
                                        ;factory
        (when factory
          (let [fm (new Method factory-name ctype ptypes)
                gen (new GeneratorAdapter (+ (. Opcodes ACC_PUBLIC) (. Opcodes ACC_STATIC)) 
                         fm nil nil cv)]
            (. gen (visitCode))
            (. gen newInstance ctype)
            (. gen dup)
            (. gen (loadArgs))
            (. gen (invokeConstructor ctype m))            
            (. gen (returnValue))
            (. gen (endMethod))))))
    
                                        ;add methods matching supers', if no fn -> call super
    (let [mm (non-private-methods super)]
      (doseq [^java.lang.reflect.Method meth (vals mm)]
             (emit-forwarding-method (.getName meth) (.getParameterTypes meth) (.getReturnType meth) false
                                     (fn [^GeneratorAdapter gen ^Method m]
                                       (. gen (loadThis))
                                        ;push args
                                       (. gen (loadArgs))
                                        ;call super
                                       (. gen (visitMethodInsn (. Opcodes INVOKESPECIAL) 
                                                               (. super-type (getInternalName))
                                                               (. m (getName))
                                                               (. m (getDescriptor)))))))
                                        ;add methods matching interfaces', if no fn -> throw
      (reduce (fn [mm ^java.lang.reflect.Method meth]
                (if (contains? mm (method-sig meth))
                  mm
                  (do
                    (emit-forwarding-method (.getName meth) (.getParameterTypes meth) (.getReturnType meth) false
                                            emit-unsupported)
                    (assoc mm (method-sig meth) meth))))
              mm (mapcat #(.getMethods ^Class %) interfaces))
                                        ;extra methods
       (doseq [[mname pclasses rclass :as msig] methods]
         (emit-forwarding-method mname pclasses rclass (:static (meta msig))
                                 emit-unsupported))
                                        ;expose specified overridden superclass methods
       (doseq [[local-mname ^java.lang.reflect.Method m] (reduce (fn [ms [[name _ _] m]]
                              (if (contains? exposes-methods (symbol name))
                                (conj ms [((symbol name) exposes-methods) m])
                                ms)) [] (concat (seq mm)
                                                (seq (protected-final-methods super))))]
         (let [ptypes (to-types (.getParameterTypes m))
               rtype (totype (.getReturnType m))
               exposer-m (new Method (str local-mname) rtype ptypes)
               target-m (new Method (.getName m) rtype ptypes)
               gen (new GeneratorAdapter (. Opcodes ACC_PUBLIC) exposer-m nil nil cv)]
           (. gen (loadThis))
           (. gen (loadArgs))
           (. gen (visitMethodInsn (. Opcodes INVOKESPECIAL) 
                                   (. super-type (getInternalName))
                                   (. target-m (getName))
                                   (. target-m (getDescriptor))))
           (. gen (returnValue))
           (. gen (endMethod)))))
                                        ;main
    (when main
      (let [m (. Method getMethod "void main (String[])")
            gen (new GeneratorAdapter (+ (. Opcodes ACC_PUBLIC) (. Opcodes ACC_STATIC)) 
                     m nil nil cv)
            no-main-label (. gen newLabel)
            end-label (. gen newLabel)]
        (. gen (visitCode))

        (emit-get-var gen main-name)
        (. gen dup)
        (. gen ifNull no-main-label)
        (.checkCast gen ifn-type)
        (. gen loadArgs)
        (. gen (invokeStatic rt-type (. Method (getMethod "clojure.lang.ISeq seq(Object)"))))
        (. gen (invokeInterface ifn-type (new Method "applyTo" obj-type 
                                              (into-array [iseq-type]))))
        (. gen pop)
        (. gen goTo end-label)
                                        ;no main found
        (. gen mark no-main-label)
        (. gen (throwException ex-type (str impl-pkg-name "/" prefix main-name " not defined")))
        (. gen mark end-label)
        (. gen (returnValue))
        (. gen (endMethod))))
                                        ;field exposers
    (doseq [[f {getter :get setter :set}] exposes]
      (let [fld (find-field super (str f))
            ftype (totype (.getType fld))
            static? (Modifier/isStatic (.getModifiers fld))
            acc (+ Opcodes/ACC_PUBLIC (if static? Opcodes/ACC_STATIC 0))]
        (when getter
          (let [m (new Method (str getter) ftype (to-types []))
                gen (new GeneratorAdapter acc m nil nil cv)]
            (. gen (visitCode))
            (if static?
              (. gen getStatic ctype (str f) ftype)
              (do
                (. gen loadThis)
                (. gen getField ctype (str f) ftype)))
            (. gen (returnValue))
            (. gen (endMethod))))
        (when setter
          (let [m (new Method (str setter) Type/VOID_TYPE (into-array [ftype]))
                gen (new GeneratorAdapter acc m nil nil cv)]
            (. gen (visitCode))
            (if static?
              (do
                (. gen loadArgs)
                (. gen putStatic ctype (str f) ftype))
              (do
                (. gen loadThis)
                (. gen loadArgs)
                (. gen putField ctype (str f) ftype)))
            (. gen (returnValue))
            (. gen (endMethod))))))
                                        ;finish class def
    (. cv (visitEnd))
    [cname (. cv (toByteArray))]))



(defn monkey [& options]
  (let [options-map (into {} (map vec (partition 2 options)))
        pname (-> options-map :name)
        [cname bytecode] (generate-class options-map)]
    (let [compile-effect (when *compile-files*
                           (clojure.lang.Compiler/writeClassFile pname bytecode))
          pc-effect (or (RT/loadClassForName pname)
                        (. ^DynamicClassLoader (deref clojure.lang.Compiler/LOADER) (defineClass pname bytecode options)))
          ]
      pc-effect)))

(defmacro defwidget [name [& fields] [& interfaces] [& signals] & methods]
  (let [[super-class interfaces] (if (isa? (first interfaces) QObject)
                                   [(first interfaces) (rest interfaces)]
                                   [QWidget interfaces])
        init-method? (fn [[mname & _]]
                              (= mname 'init))
        init-method (first (filter init-method? methods))
        methods (remove init-method? methods)
        post-init-sym (gensym "post-init")
        sclass (fn [sname scount]
                 (symbol (str "com.trolltech.qt.QSignalEmitter$Signal" scount)))
        this-sym (gensym "this")
        init-method-sym (gensym "init")
        fname (last (clojure.string/split (clojure.core/name name) #"\."))
        ns-sym (gensym "ns")]

    `(let [~ns-sym (create-ns (gensym ~(str name)))
]

       

       ~@(when init-method
           [`(intern ~ns-sym (quote ~init-method-sym)
                     (fn ~@(rest init-method)))])

       (intern
        ~ns-sym
        (quote ~post-init-sym)
        (fn [~this-sym]
          (try
            ~@(for [[sname scount] signals]
                `(let [signal# (new ~(sclass sname scount) ~this-sym)
                       types-field# (.getDeclaredField com.trolltech.qt.internal.QSignalEmitterInternal$AbstractSignalInternal "types")
                       array-dimensions-field# (.getDeclaredField com.trolltech.qt.internal.QSignalEmitterInternal$AbstractSignalInternal "arrayDimensions")]
                   (.setAccessible types-field# true)
                   (.set types-field# signal# (into-array Class [~@(repeat scount 'Object)]))

                   (.setAccessible array-dimensions-field# true)
                   (.set array-dimensions-field# signal# (int-array [~@(repeat scount 0)]))

                   (set! (. ~this-sym ~sname) signal#)))

            ~@(when init-method
                [`((ns-resolve ~ns-sym (quote ~init-method-sym)) ~this-sym)])
            (catch Exception e#
              (~'msg e#) )))
         )

       ~@(for [[mname & fn-spec] methods]
           `(intern ~ns-sym (quote ~mname)
                    (fn ~mname ~@fn-spec)))

       (let [wclass# (monkey
                      :name  ~fname
                      :extends ~super-class
                      :load-impl-ns false
                      :prefix ""
                      :impl-ns (str (clojure.core/ns-name ~ns-sym))
                      :field-names
                      [~@(for [[sname scount] signals]
                           `(with-meta  '~sname
                              {:class ~(sclass sname scount)}))
                       ~@(map clojure.core/name fields)]
                      ;; :methods [["setText" [String] Void/TYPE]]
                      :post-init ~(clojure.core/name post-init-sym)
                      :implements [~@interfaces]
                      )]
         (def impl-ns# ~ns-sym)
         (def ~(symbol (str name "-class")) wclass#)
         (defn ~(symbol fname) []
           (.newInstance wclass#))
         wclass#))))



