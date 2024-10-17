#!/bin/sh
set -ex

TIME=5s

go test ./fuzz -fuzztime ${TIME} -fuzz="^FuzzAdd2$"
go test ./fuzz -fuzztime ${TIME} -fuzz="^FuzzAdd$"
go test ./fuzz -fuzztime ${TIME} -fuzz="^FuzzAssign$"
go test ./fuzz -fuzztime ${TIME} -fuzz="^FuzzIf2$"
go test ./fuzz -fuzztime ${TIME} -fuzz="^FuzzIf3$"
go test ./fuzz -fuzztime ${TIME} -fuzz="^FuzzIf$"
go test ./fuzz -fuzztime ${TIME} -fuzz="^FuzzNest$"
go test ./fuzz -fuzztime ${TIME} -fuzz="^FuzzReturn$"
