console.log("loading...");

var ALL_EVENTS = ['onCopy', 'onCut', 'onPaste', 'onKeyDown', 'onKeyPress', 'onKeyUp', 'onFocus', 'onBlur', 'onChange', 'onInput', 'onSubmit', 'onClick', 'onDoubleClick', 'onDrag', 'onDragEnd', 'onDragEnter', 'onDragExit', 'onDragLeave', 'onDragOver', 'onDragStart', 'onDrop', 'onMouseDown', 'onMouseEnter', 'onMouseLeave', 'onMouseMove', 'onMouseOut', 'onMouseOver', 'onMouseUp', 'onTouchCancel', 'onTouchEnd', 'onTouchMove', 'onTouchStart', 'onScroll', 'onWheel'];

var MyInput = React.createClass({
    getInitialState: function(){
        return {value: this.props.attrs.value};
    },
    render: function() {

        var tag = this.props.tag;
        var attrs = this.props.attrs;
        var myAttributes = {"data-testinput" : "whoo2"};

        if ( attrs ){
            delete attrs['value'];
            attrs['defaultValue'] = this.state.value;
            if ( attrs['key'] ){
                myAttributes['key'] = 'div-' + attrs['key'];
            }
        }

        for (var attrName in attrs){
            if ( attrName == 'onChange' ){
                var handler = attrs[attrName];
                attrs[attrName] = function(e){
                    this.setState({value: e.target.value});
                    e.target.focus();
                    return handler(e);
                }.bind(this);
            }
        }
        myAttributes.key = 'yarrr';
        console.log(myAttributes);
        
        return React.createElement('div',myAttributes,React.createElement(tag, attrs));

    }
});
// function MyInput(_, elem){ return elem};

function convert_ui(html){
    // console.log("converting... " + JSON.stringify(html) + ( typeof html == 'string' || html instanceof String));
    if ( typeof html == 'string' || html instanceof String){
        return html;
    }

    var tag = html[0];
    var attrs = html[1];

    var events = ALL_EVENTS;
    for (var attrName in attrs){
        if ( events.indexOf(attrName) >= 0){
            attrs[attrName] = eval('(' + attrs[attrName] + ')');
        }
    }

    var children_html = html[2];
    attrs['children'] = [];
    for (var i = 0; i < children_html.length; i ++){
        attrs['children'].push(convert_ui(children_html[i]));
    }

    var elem = React.createElement(tag, attrs);

    // if ( ['input','textarea'].indexOf(tag) >= 0){
    //     elem = MyInput({tag: tag, attrs: attrs});
    // }

    return elem;
}

function updatehtml(html){
    console.log('updating html');
    var root = convert_ui(html);
    

    React.render(root, document.getElementById('root'));

}

function send(){
    try{
        window.channels.put( JSON.stringify(Array.prototype.slice.call(arguments) ));
    }catch (e){
        alert(e + '');
    }
}

function setval(name, val){
    try{
        window.channels.setval( JSON.stringify(Array.prototype.slice.call(arguments)) );
    }catch (e){
        alert(e + '');
    }    
}
