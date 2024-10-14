package ast

import (
	"testing"

	"github.com/davecgh/go-spew/spew"
	"github.com/stretchr/testify/assert"
)

func TestTranslate(t *testing.T) {
	spew.Config.DisablePointerAddresses = true
	spew.Config.SortKeys = true
	spew.Config.DisableCapacities = true

	f, err := TranslateFile("../samples/nest.go")
	assert.Nil(t, err)
	spew.Dump(f)
}
