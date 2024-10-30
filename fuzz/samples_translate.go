package fuzz

func entrypoint_add2_translate (
  a uint8,
  b uint8,
) (
  _out0 uint8,
) {
{ // b0
  _out0 = (a + b) + b
  goto _endblock0
}
_endblock0:
return
}


func entrypoint_add_translate (
  a uint8,
  b uint8,
) (
  _out0 uint8,
) {
{ // b0
  _out0 = a + b
  goto _endblock0
}
_endblock0:
return
}


func entrypoint_assign_translate (
  a uint8,
) (
  _out0 uint8,
) {
{ // b0
  a = a + 1
  a = a + 2
  a = a + 3
  _out0 = a
  goto _endblock0
}
_endblock0:
return
}


func entrypoint_define_translate (
  a uint8,
) (
  _out0 uint8,
) {
{ // b0
  var b uint8
  _ = b
  b = a + 1
  _out0 = b
  goto _endblock0
}
_endblock0:
return
}


func entrypoint_if2_translate (
  a uint8,
  b uint8,
) (
  _out0 uint8,
) {
{ // b0
  var x uint8
  _ = x
  x = a
  if a > 5 { // b1
    if a > 10 { // b2
      x = x + 2
    }
  }
  _out0 = x
  goto _endblock0
}
_endblock0:
return
}


func entrypoint_if3_translate (
  a uint8,
) (
  _out0 uint8,
) {
{ // b0
  var x uint8
  _ = x
  x = a
  if (a / 2) == 0 { // b1
    if (a / 3) == 0 { // b2
      x = x + 2
    } else if x == 0 { // b3
      x = x + 3
    }
  }
  _out0 = x
  goto _endblock0
}
_endblock0:
return
}


func entrypoint_if4_translate (
  a uint8,
) (
  _out0 uint8,
) {
{ // b0
  var i uint8
  _ = i
  i = 0
  i = 1
  if (a / 2) == 0 { // b1
    var j uint8
    _ = j
    j = 0
    if (a / 3) == 0 { // b2
      i = i + 2
      j = i
      if (a / 4) == 0 { // b3
        i = (i + 4) + j
      }
    } else { // b4
      i = i + 3
    }
    i = i + 4
  }
  i = i + 5
  _out0 = i
  goto _endblock0
}
_endblock0:
return
}


func entrypoint_if5_translate (
  a uint8,
) (
  _out0 uint8,
) {
{ // b0
  var x uint8
  _ = x
  x = a
  if (a / 2) == 0 { // b1
    x = x + 1
  } else if (a / 3) == 0 { // b2
    x = x + 2
  } else if (a / 5) == 0 { // b3
    x = x + 3
  } else { // b4
    x = x + 4
  }
  _out0 = x
  goto _endblock0
}
_endblock0:
return
}


func entrypoint_if_translate (
  a uint8,
  b uint8,
) (
  _out0 uint8,
) {
{ // b0
  var x uint8
  _ = x
  x = 0
  if a == 0 { // b1
    x = x + b
    x = x + 2
    var x uint8
    _ = x
    x = 0
    x = 7
    b = x
  }
  _out0 = x
  goto _endblock0
}
_endblock0:
return
}


func entrypoint_nest_translate (
  a uint8,
  b uint8,
) (
  _out0 uint8,
  _out1 uint16,
) {
{ // b0
  var x uint8
  _ = x
  x = 0
  var y uint16
  _ = y
  y = 0
  { // b1
    b = 44
    var b uint8
    _ = b
    b = 32
    { // b2
      b = 88
      var b uint16
      _ = b
      b = 33
      y = b + 2
    }
    x = a + b
    b = 55
  }
  b = b + 1
  _out0 = x
  _out1 = y
  goto _endblock0
}
_endblock0:
return
}


func entrypoint_return_translate (
  a uint8,
) (
  _out0 uint8,
) {
{ // b0
  _out0 = a
  goto _endblock0
}
_endblock0:
return
}


func entrypoint_for_translate (
  a uint8,
) (
  _out0 uint8,
) {
{ // b0
  {
    var i int32
    _ = i
    i = 0
    for i < 10 { // b1
      { // b2
        a = a + 1
      }
      i = i + 1
    }
  }
  _out0 = a
  goto _endblock0
}
_endblock0:
return
}


func entrypoint_for2_translate (
  a uint8,
) (
  _out0 uint8,
) {
{ // b0
  {
    var i int32
    _ = i
    i = 0
    for i < 4 { // b1
      { // b2
        a = a + 1
        if a > 19 { // b3
          break
        }
        a = a + 1
      }
      i = i + 1
    }
  }
  _out0 = a
  goto _endblock0
}
_endblock0:
return
}


