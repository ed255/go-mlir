package fuzz

func entrypoint_add2_translate (
  a uint8,
  b uint8,
) (
  _out0 uint8,
) {
  _out0 = (a + b) + b
  return
}


func entrypoint_add_translate (
  a uint8,
  b uint8,
) (
  _out0 uint8,
) {
  _out0 = a + b
  return
}


func entrypoint_assign_translate (
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


func entrypoint_define_translate (
  a uint8,
) (
  _out0 uint8,
) {
  var b uint8
  b = a + 1
  _out0 = b
  return
}


func entrypoint_func_translate (
  a uint8,
  b uint8,
) (
  _out0 uint8,
) {
  _out0 = add_func_translate(a, b)
  return
}

func add_func_translate (
  a uint8,
  b uint8,
) (
  _out0 uint8,
) {
  _out0 = a + b
  return
}


func entrypoint_func2_translate (
  a uint8,
  b uint8,
) (
  _out0 uint8,
) {
  var x uint8
  var y uint8
  x, y = add_func2_translate(a, b)
  _out0 = x + y
  return
}

func add_func2_translate (
  a uint8,
  b uint8,
) (
  _out0 uint8,
  _out1 uint8,
) {
  _out0 = a
  _out1 = a + b
  return
}


func entrypoint_if2_translate (
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


func entrypoint_if3_translate (
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


func entrypoint_if_translate (
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


func entrypoint_nest_translate (
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


func entrypoint_return_translate (
  a uint8,
) (
  _out0 uint8,
) {
  _out0 = a
  return
}


func entrypoint_struct_translate (
  a uint8,
  b uint8,
) (
  _out0 uint8,
) {
  var d Data_struct
  d = Data_struct{a: a, b: b}
  d.a = d.a + 1
  var r uint8
  r = add_struct_translate(d)
  _out0 = r
  return
}

func add_struct_translate (
  d Data_struct,
) (
  _out0 uint8,
) {
  _out0 = d.a + d.b
  return
}


func entrypoint_var_translate (
  a uint8,
) (
  _out0 uint8,
) {
  var b uint8
  b = uint8(0)
  b = a + 1
  _out0 = b
  return
}


func entrypoint_for_translate (
  a uint8,
) (
  _out0 uint8,
) {
  {
    var i int32
    i = 0
    for i < 10 {
      {
        a = a + 1
      }
      i = i + 1
    }
  }
  _out0 = a
  return
}


func entrypoint_for2_translate (
  a uint8,
) (
  _out0 uint8,
) {
  {
    var i int32
    i = 0
    for i < 10 {
      {
        a = a + 1
        if a > 19 {
          break
        }
      }
      i = i + 1
    }
  }
  _out0 = a
  return
}


