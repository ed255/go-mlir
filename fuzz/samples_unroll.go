package fuzz

func entrypoint_add2_unroll (
  a uint8,
  b uint8,
) (
  _out0 uint8,
) {
{ // b1
  _out0 = (a + b) + b
  goto _endblock1
}
_endblock1:
return
}


func entrypoint_add_unroll (
  a uint8,
  b uint8,
) (
  _out0 uint8,
) {
{ // b1
  _out0 = a + b
  goto _endblock1
}
_endblock1:
return
}


func entrypoint_assign_unroll (
  a uint8,
) (
  _out0 uint8,
) {
{ // b1
  a = a + 1
  a = a + 2
  a = a + 3
  _out0 = a
  goto _endblock1
}
_endblock1:
return
}


func entrypoint_define_unroll (
  a uint8,
) (
  _out0 uint8,
) {
{ // b1
  var b uint8
  _ = b
  b = a + 1
  _out0 = b
  goto _endblock1
}
_endblock1:
return
}


func entrypoint_if2_unroll (
  a uint8,
  b uint8,
) (
  _out0 uint8,
) {
{ // b1
  var x uint8
  _ = x
  x = a
  if a > 5 { // b2
    if a > 10 { // b3
      x = x + 2
    }
  }
  _out0 = x
  goto _endblock1
}
_endblock1:
return
}


func entrypoint_if3_unroll (
  a uint8,
) (
  _out0 uint8,
) {
{ // b1
  var x uint8
  _ = x
  x = a
  if (a / 2) == 0 { // b2
    if (a / 3) == 0 { // b3
      x = x + 2
    } else if x == 0 { // b4
      x = x + 3
    }
  }
  _out0 = x
  goto _endblock1
}
_endblock1:
return
}


func entrypoint_if4_unroll (
  a uint8,
) (
  _out0 uint8,
) {
{ // b1
  var i uint8
  _ = i
  // i = 0
  i = 0
  // i = 1
  i = 1
  if (a / 2) == 0 { // b2
    var j uint8
    _ = j
    // j = 0
    j = 0
    if (a / 3) == 0 { // b3
      // i = i + 2
      i = 3
      // j = i
      j = 3
      if (a / 4) == 0 { // b4
        // i = (i + 4) + j
        i = 10
      }
    } else { // b5
      // i = i + 3
      i = 4
    }
    i = i + 4
  }
  i = i + 5
  _out0 = i
  goto _endblock1
}
_endblock1:
return
}


func entrypoint_if5_unroll (
  a uint8,
) (
  _out0 uint8,
) {
{ // b1
  var x uint8
  _ = x
  x = a
  if (a / 2) == 0 { // b2
    x = x + 1
  } else if (a / 3) == 0 { // b3
    x = x + 2
  } else if (a / 5) == 0 { // b4
    x = x + 3
  } else { // b5
    x = x + 4
  }
  _out0 = x
  goto _endblock1
}
_endblock1:
return
}


func entrypoint_if_unroll (
  a uint8,
  b uint8,
) (
  _out0 uint8,
) {
{ // b1
  var x uint8
  _ = x
  // x = 0
  x = 0
  if a == 0 { // b2
    x = 0 + b
    x = x + 2
    var x uint8
    _ = x
    // x = 0
    x = 0
    // x = 7
    x = 7
    // b = x
    b = 7
  }
  _out0 = x
  goto _endblock1
}
_endblock1:
return
}


func entrypoint_nest_unroll (
  a uint8,
  b uint8,
) (
  _out0 uint8,
  _out1 uint16,
) {
{ // b1
  var x uint8
  _ = x
  // x = 0
  x = 0
  var y uint16
  _ = y
  // y = 0
  y = 0
  { // b2
    // b = 44
    b = 44
    var b uint8
    _ = b
    // b = 32
    b = 32
    { // b3
      // b = 88
      b = 88
      var b uint16
      _ = b
      // b = 33
      b = 33
      // y = b + 2
      y = 35
    }
    x = a + 88
    // b = 55
    b = 55
  }
  // b = b + 1
  b = 45
  _out0 = x
  // _out1 = y
  _out1 = 35
  goto _endblock1
}
_endblock1:
return
}


func entrypoint_return_unroll (
  a uint8,
) (
  _out0 uint8,
) {
{ // b1
  _out0 = a
  goto _endblock1
}
_endblock1:
return
}


func entrypoint_for_unroll (
  a uint8,
) (
  _out0 uint8,
) {
{ // b1
  { // b2
    var i int32
    _ = i
    // i = 0
    i = 0
    { // b3
      { // b4
        a = a + 1
      }
      // i = i + 1
      i = 1
    }
    { // b5
      { // b6
        a = a + 1
      }
      // i = i + 1
      i = 2
    }
    { // b7
      { // b8
        a = a + 1
      }
      // i = i + 1
      i = 3
    }
    { // b9
      { // b10
        a = a + 1
      }
      // i = i + 1
      i = 4
    }
    { // b11
      { // b12
        a = a + 1
      }
      // i = i + 1
      i = 5
    }
    { // b13
      { // b14
        a = a + 1
      }
      // i = i + 1
      i = 6
    }
    { // b15
      { // b16
        a = a + 1
      }
      // i = i + 1
      i = 7
    }
    { // b17
      { // b18
        a = a + 1
      }
      // i = i + 1
      i = 8
    }
    { // b19
      { // b20
        a = a + 1
      }
      // i = i + 1
      i = 9
    }
    { // b21
      { // b22
        a = a + 1
      }
      // i = i + 1
      i = 10
    }
  }
  _out0 = a
  goto _endblock1
}
_endblock1:
return
}


func entrypoint_for2_unroll (
  a uint8,
) (
  _out0 uint8,
) {
{ // b1
  { // b2
    var i int32
    _ = i
    // i = 0
    i = 0
    { // b3
      { // b4
        a = a + 1
        if a > 19 { // b5
          goto _endblock2
        }
        a = a + 1
      }
      // i = i + 1
      i = 1
    }
    { // b6
      { // b7
        a = a + 1
        if a > 19 { // b8
          goto _endblock2
        }
        a = a + 1
      }
      // i = i + 1
      i = 2
    }
    { // b9
      { // b10
        a = a + 1
        if a > 19 { // b11
          goto _endblock2
        }
        a = a + 1
      }
      // i = i + 1
      i = 3
    }
    { // b12
      { // b13
        a = a + 1
        if a > 19 { // b14
          goto _endblock2
        }
        a = a + 1
      }
      // i = i + 1
      i = 4
    }
  }
_endblock2:
  _out0 = a
  goto _endblock1
}
_endblock1:
return
}


