package main

import (
	"fmt"
	"log"
	"os"
	"path"
	"regexp"
	"strings"
	"text/template"

	"frontend/ast"

	"github.com/iancoleman/strcase"
)

type FuzzTestData struct {
	NameCapital string
	Name        string
	InputSample string
	InputSpec   string
	Input       string
	OutputLen   int
}

const tmplFuzzTestStr = `func Fuzz{{.NameCapital}}(f *testing.F) {
	f.Add({{.InputSample}})
	f.Fuzz(func(t *testing.T, {{.InputSpec}}) {
		{{out .OutputLen ""}} := entrypoint_{{.Name}}({{.Input}})
		{{out .OutputLen "_t1"}} := entrypoint_{{.Name}}_t1({{.Input}})
		{{range $i, $id := count .OutputLen }}
		assert.Equal(t, out{{$id}}, out{{$id}}_t1){{end}}
	})
}
`

func errcheck(err error) {
	if err != nil {
		log.Fatal(err)
	}
}

func suffixGlobals(content string, suffix string) string {
	funcRe := regexp.MustCompile(`func ([^ ]*)[ ]*\(`)
	typeRe := regexp.MustCompile(`type ([^ ]*)`)

	funcReMatches := funcRe.FindAllStringSubmatch(content, -1)
	for _, funcReMatch := range funcReMatches {
		funcName := funcReMatch[1]
		content = strings.ReplaceAll(content, funcName, fmt.Sprintf("%v_%v", funcName, suffix))
	}
	typeReMatches := typeRe.FindAllStringSubmatch(content, -1)
	for _, typeReMatch := range typeReMatches {
		typeName := typeReMatch[1]
		content = strings.ReplaceAll(content, typeName, fmt.Sprintf("%v_%v", typeName, suffix))
	}
	return content
}
func main() {
	const pathSamples = "./samples"
	const pathFuzzTest = "./fuzz/fuzz_test.go"
	const pathFuzzSamples = "./fuzz/samples.go"
	const pathFuzzSamplesT1 = "./fuzz/samples_t1.go"
	const pathFuzzScript = "./fuzz.sh"

	tmplFuzzTest, err := template.New("test").Funcs(map[string]any{
		"count": func(n int) []int {
			c := make([]int, n)
			for i := 0; i < n; i++ {
				c[i] = i
			}
			return c
		},
		"out": func(n int, suffix string) string {
			s := ""
			for i := 0; i < n; i++ {
				if i != 0 {
					s += ", "
				}
				s += fmt.Sprintf("out%v%v", i, suffix)
			}
			return s
		},
	}).Parse(tmplFuzzTestStr)
	errcheck(err)

	fileFuzzTest, err := os.OpenFile(pathFuzzTest, os.O_RDWR|os.O_CREATE|os.O_TRUNC, 0644)
	errcheck(err)
	defer fileFuzzTest.Close()
	fileFuzzSamples, err := os.OpenFile(pathFuzzSamples, os.O_RDWR|os.O_CREATE|os.O_TRUNC, 0644)
	errcheck(err)
	defer fileFuzzSamples.Close()
	fileFuzzSamplesT1, err := os.OpenFile(pathFuzzSamplesT1, os.O_RDWR|os.O_CREATE|os.O_TRUNC, 0644)
	errcheck(err)
	defer fileFuzzSamplesT1.Close()
	fileFuzzScript, err := os.OpenFile(pathFuzzScript, os.O_RDWR|os.O_CREATE|os.O_TRUNC, 0755)
	errcheck(err)
	defer fileFuzzScript.Close()

	for _, file := range []*os.File{fileFuzzTest, fileFuzzSamples, fileFuzzSamplesT1} {
		file.WriteString("package fuzz\n\n")
	}

	fileFuzzTest.WriteString(`import (
	"testing"

	"github.com/stretchr/testify/assert"
)

`)

	fileFuzzScript.WriteString(`#!/bin/sh
set -ex

TIME=5s

`)

	// samples, err := os.ReadDir(pathSamples)
	// errcheck(err)

	samples := []string{
		"add2.go",
		"add.go",
		"assign.go",
		// "define.go",
		// "func.go",
		// "func2.go",
		"if2.go",
		"if3.go",
		"if.go",
		"nest.go",
		"return.go",
		// "struct.go",
		// "var.go",
	}

	inputSampleMap := make(map[string]string)
	inputSpecMap := make(map[string]string)
	inputMap := make(map[string]string)
	outputLenMap := make(map[string]int)

	for _, name := range []string{"assign", "if3", "return"} {
		inputSampleMap[name] = "uint8(128)"
		inputSpecMap[name] = "a uint8"
		inputMap[name] = "a"
		outputLenMap[name] = 1
	}
	for _, name := range []string{"add2", "add", "if2", "if"} {
		inputSampleMap[name] = "uint8(128), uint8(128)"
		inputSpecMap[name] = "a uint8, b uint8"
		inputMap[name] = "a, b"
		outputLenMap[name] = 1
	}
	for _, name := range []string{"nest"} {
		inputSampleMap[name] = "uint8(128), uint8(128)"
		inputSpecMap[name] = "a uint8, b uint8"
		inputMap[name] = "a, b"
		outputLenMap[name] = 2
	}

	for _, s := range samples {
		// sampleFileName := s.Name()
		sampleFileName := s
		fmt.Printf(">>> %v\n", sampleFileName)
		samplePath := path.Join(pathSamples, sampleFileName)
		sampleName := strings.TrimSuffix(sampleFileName, ".go")
		contentBytes, err := os.ReadFile(samplePath)
		errcheck(err)
		sampleContent := string(contentBytes)
		sampleContent = suffixGlobals(sampleContent, sampleName)

		f, err := ast.TranslateFile(samplePath, sampleContent)
		errcheck(err)

		var t1Str strings.Builder
		p := ast.NewPrinter(&t1Str, ast.PrinterOpts{GoCompat: true})
		p.File(&f)

		sampleContentT1 := t1Str.String()
		sampleContentT1 = suffixGlobals(sampleContentT1, "t1")

		var fuzzTestStr strings.Builder
		err = tmplFuzzTest.Execute(&fuzzTestStr, FuzzTestData{
			NameCapital: strcase.ToCamel(sampleName),
			Name:        sampleName,
			InputSample: inputSampleMap[sampleName],
			InputSpec:   inputSpecMap[sampleName],
			Input:       inputMap[sampleName],
			OutputLen:   outputLenMap[sampleName],
		})
		errcheck(err)

		sampleContent = strings.ReplaceAll(sampleContent, "package main\n", "")
		fileFuzzSamples.WriteString(sampleContent)
		fileFuzzSamplesT1.WriteString(sampleContentT1)
		fileFuzzSamplesT1.WriteString("\n")
		fileFuzzTest.WriteString(fuzzTestStr.String())
		fileFuzzTest.WriteString("\n")
		fileFuzzScript.WriteString(fmt.Sprintf(
			"go test ./fuzz -fuzztime ${TIME} -fuzz=\"^Fuzz%v$\"\n",
			strcase.ToCamel(sampleName),
		))
	}
}
