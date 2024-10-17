package fuzz

func entrypoint_add2_t1 (
  a uint8,
  b uint8,
) (
  _out0 uint8,
) {
  _out0 = (a + b) + b
  return
}

func entrypoint_add_t1 (
  a uint8,
  b uint8,
) (
  _out0 uint8,
) {
  _out0 = a + b
  return
}

func entrypoint_assign_t1 (
  a uint8,
) (
  _out0 uint8,
) {
  a = a + 1
  a = a + 2
  a = a + 3
  _out0 = a
  return
}

func entrypoint_if2_t1 (
  a uint8,
  b uint8,
) (
  _out0 uint8,
) {
  var x_b1 uint8
  x_b1 = 0
  var _0_b1 bool
  _0_b1 = a > 5
    // (bid=2) if a > 5
    var _1_b2 bool
    _1_b2 = a > 10
      // (bid=3) if a > 10
      var x_b1_c3 uint8
      x_b1_c3 = x_b1 + 2
    var x_b1_c2 uint8
    if _1_b2 { x_b1_c2 = x_b1_c3 } else { x_b1_c2 = x_b1 }
  if _0_b1 { x_b1 = x_b1_c2 } else { x_b1 = x_b1 }
  _out0 = x_b1
  return
}

func entrypoint_if3_t1 (
  a uint8,
) (
  _out0 uint8,
) {
  var x_b1 uint8
  x_b1 = 0
  var _0_b1 bool
  _0_b1 = (a / 2) == 0
    // (bid=2) if (a / 2) == 0
    var _1_b2 bool
    _1_b2 = (a / 3) == 0
      // (bid=3) if (a / 3) == 0
      var x_b1_c3 uint8
      x_b1_c3 = x_b1 + 2
      // (bid=4) else
      var x_b1_c4 uint8
      x_b1_c4 = x_b1 + 3
    var x_b1_c2 uint8
    if _1_b2 { x_b1_c2 = x_b1_c3 } else { x_b1_c2 = x_b1_c4 }
  if _0_b1 { x_b1 = x_b1_c2 } else { x_b1 = x_b1 }
  _out0 = x_b1
  return
}

func entrypoint_if_t1 (
  a uint8,
  b uint8,
) (
  _out0 uint8,
) {
  var x_b1 uint8
  x_b1 = 0
  var _0_b1 bool
  _0_b1 = a == 0
    // (bid=2) if a == 0
    var x_b1_c2 uint8
    x_b1_c2 = x_b1 + b
    x_b1_c2 = x_b1_c2 + 2
    var x_b2 uint8
    x_b2 = 7
    var b_c2 uint8
    b_c2 = x_b2
  if _0_b1 { x_b1 = x_b1_c2 } else { x_b1 = x_b1 }
  if _0_b1 { b = b_c2 } else { b = b }
  _out0 = x_b1
  return
}

func entrypoint_nest_t1 (
  a uint8,
  b uint8,
) (
  _out0 uint8,
  _out1 uint16,
) {
  var x_b1 uint8
  x_b1 = 0
  var y_b1 uint16
  y_b1 = 0
    b = 44
    var b_b2 uint8
    b_b2 = 32
      b_b2 = 88
      var b_b3 uint16
      b_b3 = 33
      y_b1 = b_b3 + 2
    x_b1 = a + b_b2
    b_b2 = 55
  b = b + 1
  _out0 = x_b1
  _out1 = y_b1
  return
}

func entrypoint_return_t1 (
  a uint8,
) (
  _out0 uint8,
) {
  _out0 = a
  return
}

