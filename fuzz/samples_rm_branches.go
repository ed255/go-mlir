package fuzz

func entrypoint_add2_rm_branches (
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


func entrypoint_add_rm_branches (
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


func entrypoint_assign_rm_branches (
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


func entrypoint_define_rm_branches (
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


func entrypoint_if2_rm_branches (
  a uint8,
  b uint8,
) (
  _out0 uint8,
) {
{ // b1
  var x uint8
  _ = x
  x = a
  var x_c2 uint8
  _ = x_c2
  x_c2 = x
  // (bid=0) if a > 5
  { // b2
    var x_c3 uint8
    _ = x_c3
    x_c3 = x
    // (bid=0) if a > 10
    { // b3
      x_c3 = x_c3 + 2
    }
    if a > 10 { x_c2 = x_c3 } else { x_c2 = x_c2 }
  }
  if a > 5 { x = x_c2 } else { x = x }
  _out0 = x
  goto _endblock1
}
_endblock1:
return
}


func entrypoint_if3_rm_branches (
  a uint8,
) (
  _out0 uint8,
) {
{ // b1
  var x uint8
  _ = x
  x = a
  var x_c2 uint8
  _ = x_c2
  x_c2 = x
  // (bid=0) if (a / 2) == 0
  { // b2
    var x_c3 uint8
    _ = x_c3
    x_c3 = x
    var x_c4 uint8
    _ = x_c4
    x_c4 = x_c2
    // (bid=0) if (a / 3) == 0
    { // b3
      x_c3 = x_c3 + 2
    }
    // (bid=0) if x_c2 == 0
    { // b4
      x_c4 = x_c4 + 3
    }
    if x_c2 == 0 { x_c2 = x_c4 } else { x_c2 = x_c2 }
    if (a / 3) == 0 { x_c2 = x_c3 } else { x_c2 = x_c2 }
  }
  if (a / 2) == 0 { x = x_c2 } else { x = x }
  _out0 = x
  goto _endblock1
}
_endblock1:
return
}


func entrypoint_if4_rm_branches (
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
  var i_c2 uint8
  _ = i_c2
  i_c2 = i
  // (bid=0) if (a / 2) == 0
  { // b2
    var j uint8
    _ = j
    // j = 0
    j = 0
    var i_c3 uint8
    _ = i_c3
    i_c3 = i
    var j_c3 uint8
    _ = j_c3
    j_c3 = j
    var i_c5 uint8
    _ = i_c5
    i_c5 = i_c2
    // (bid=0) if (a / 3) == 0
    { // b3
      // i = i + 2
      i_c3 = 3
      // j = i
      j_c3 = 3
      var i_c4 uint8
      _ = i_c4
      i_c4 = i_c3
      // (bid=0) if (a / 4) == 0
      { // b4
        // i = (i + 4) + j
        i_c4 = 10
      }
      if (a / 4) == 0 { i_c3 = i_c4 } else { i_c3 = i_c3 }
    }
    { // b5
      // i = i + 3
      i_c5 = 4
    }
    if (a / 3) == 0 { i_c2 = i_c3 } else { i_c2 = i_c5 }
    if (a / 3) == 0 { j = j_c3 } else { j = j }
    i_c2 = i_c2 + 4
  }
  if (a / 2) == 0 { i = i_c2 } else { i = i }
  i = i + 5
  _out0 = i
  goto _endblock1
}
_endblock1:
return
}


func entrypoint_if5_rm_branches (
  a uint8,
) (
  _out0 uint8,
) {
{ // b1
  var x uint8
  _ = x
  x = a
  var x_c2 uint8
  _ = x_c2
  x_c2 = x
  var x_c3 uint8
  _ = x_c3
  x_c3 = x
  var x_c4 uint8
  _ = x_c4
  x_c4 = x
  var x_c5 uint8
  _ = x_c5
  x_c5 = x
  // (bid=0) if (a / 2) == 0
  { // b2
    x_c2 = x_c2 + 1
  }
  // (bid=0) if (a / 3) == 0
  { // b3
    x_c3 = x_c3 + 2
  }
  // (bid=0) if (a / 5) == 0
  { // b4
    x_c4 = x_c4 + 3
  }
  { // b5
    x_c5 = x_c5 + 4
  }
  if (a / 5) == 0 { x = x_c4 } else { x = x_c5 }
  if (a / 3) == 0 { x = x_c3 } else { x = x }
  if (a / 2) == 0 { x = x_c2 } else { x = x }
  _out0 = x
  goto _endblock1
}
_endblock1:
return
}


func entrypoint_if_rm_branches (
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
  var x_c2 uint8
  _ = x_c2
  x_c2 = x
  var b_c2 uint8
  _ = b_c2
  b_c2 = b
  // (bid=0) if a == 0
  { // b2
    x_c2 = 0 + b
    x_c2 = x_c2 + 2
    var x uint8
    _ = x
    // x = 0
    x = 0
    // x = 7
    x = 7
    // b = x
    b_c2 = 7
  }
  if a == 0 { b = b_c2 } else { b = b }
  if a == 0 { x = x_c2 } else { x = x }
  _out0 = x
  goto _endblock1
}
_endblock1:
return
}


func entrypoint_nest_rm_branches (
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


func entrypoint_return_rm_branches (
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


func entrypoint_for_rm_branches (
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


func entrypoint_for2_rm_branches (
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
        // (bid=0) if a > 19
        { // b5
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
        // (bid=0) if a > 19
        { // b8
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
        // (bid=0) if a > 19
        { // b11
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
        // (bid=0) if a > 19
        { // b14
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


