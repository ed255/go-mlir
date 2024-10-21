#!/bin/sh
set -ex

TIME=5s

go test ./fuzz -fuzztime ${TIME} -fuzz="^FuzzTranslateAdd2$"
go test ./fuzz -fuzztime ${TIME} -fuzz="^FuzzTranslateAdd$"
go test ./fuzz -fuzztime ${TIME} -fuzz="^FuzzTranslateAssign$"
go test ./fuzz -fuzztime ${TIME} -fuzz="^FuzzTranslateDefine$"
go test ./fuzz -fuzztime ${TIME} -fuzz="^FuzzTranslateFunc$"
go test ./fuzz -fuzztime ${TIME} -fuzz="^FuzzTranslateFunc2$"
go test ./fuzz -fuzztime ${TIME} -fuzz="^FuzzTranslateIf2$"
go test ./fuzz -fuzztime ${TIME} -fuzz="^FuzzTranslateIf3$"
go test ./fuzz -fuzztime ${TIME} -fuzz="^FuzzTranslateIf$"
go test ./fuzz -fuzztime ${TIME} -fuzz="^FuzzTranslateNest$"
go test ./fuzz -fuzztime ${TIME} -fuzz="^FuzzTranslateReturn$"
go test ./fuzz -fuzztime ${TIME} -fuzz="^FuzzTranslateStruct$"
go test ./fuzz -fuzztime ${TIME} -fuzz="^FuzzTranslateVar$"
