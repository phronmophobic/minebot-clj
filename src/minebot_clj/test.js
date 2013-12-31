var MSB = 0x80
  , REST = 0x7F

function read(buf, offset) {
  var res = 0
    , offset = offset || 0
    , shift = 0
    , counter = offset
    , b
  
  do {
    b = buf[counter++]
      print(b + " " + (b & REST));
      
    res += shift < 28
      ? (b & REST) << shift
      : (b & REST) * Math.pow(2, shift)
    shift += 7
      print(res)
  } while (b >= MSB)
  
  read.bytesRead = counter - offset
  
  return res
}

print(read([247 , 1], 0));
