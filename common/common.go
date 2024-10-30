package common

import "fmt"

func Assert(check bool, msg ...string) {
	if !check {
		if len(msg) == 0 {
			panic(fmt.Errorf("assert failed"))
		} else {
			panic(fmt.Errorf("assert failed: %v", msg[0]))
		}
	}
}
