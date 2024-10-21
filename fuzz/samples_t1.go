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
  var x uint8
  x = 0
  if a > 5 {
    if a > 10 {
      x = x + 2
    }
  }
  _out0 = x
  return
}

func entrypoint_if3_t1 (
  a uint8,
) (
  _out0 uint8,
) {
  var x uint8
  x = 0
  if (a / 2) == 0 {
    if (a / 3) == 0 {
      x = x + 2
    } else if x == 0 {
      x = x + 3
    }
  }
  _out0 = x
  return
}

func entrypoint_if_t1 (
  a uint8,
  b uint8,
) (
  _out0 uint8,
) {
  var x uint8
  x = 0
  if a == 0 {
    x = x + b
    x = x + 2
    var x uint8
    x = 7
    b = x
  }
  _out0 = x
  return
}

func entrypoint_nest_t1 (
  a uint8,
  b uint8,
) (
  _out0 uint8,
  _out1 uint16,
) {
  var x uint8
  x = 0
  var y uint16
  y = 0
  {
    b = 44
    var b uint8
    b = 32
    {
      b = 88
      var b uint16
      b = 33
      y = b + 2
    }
    x = a + b
    b = 55
  }
  b = b + 1
  _out0 = x
  _out1 = y
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

