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
	PassCapital string
	Pass        string
	InputSample string
	InputSpec   string
	Input       string
	OutputLen   int
}

const tmplFuzzTestStr = `func Fuzz{{.PassCapital}}{{.NameCapital}}(f *testing.F) {
	f.Add({{.InputSample}})
	f.Fuzz(func(t *testing.T, {{.InputSpec}}) {
		{{out .OutputLen ""}} := entrypoint_{{.Name}}({{.Input}})
		{{out .OutputLen (print "_" .Pass)}} := entrypoint_{{.Name}}_{{.Pass}}({{.Input}})
		{{range $i, $id := (count .OutputLen) }}
		assert.Equal(t, out{{$id}}, out{{$id}}_{{$.Pass}}){{end}}
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

type File struct {
	Path string
	*os.File
}

func main() {
	const pathSamples = "./samples"

	fuzzTest := File{Path: "./fuzz/fuzz_test.go"}
	fuzzSamples := File{Path: "./fuzz/samples.go"}
	fuzzSamplesTranslate := File{Path: "./fuzz/samples_translate.go"}
	fuzzScript := File{Path: "./fuzz.sh"}

	var err error
	for _, file := range []*File{&fuzzTest, &fuzzSamples, &fuzzSamplesTranslate} {
		file.File, err = os.OpenFile(file.Path, os.O_RDWR|os.O_CREATE|os.O_TRUNC, 0644)
		errcheck(err)
		defer file.File.Close()
		file.File.WriteString("package fuzz\n\n")
	}

	fuzzScript.File, err = os.OpenFile(fuzzScript.Path, os.O_RDWR|os.O_CREATE|os.O_TRUNC, 0755)
	errcheck(err)
	defer fuzzScript.File.Close()

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

	fuzzTest.WriteString(`import (
	"testing"

	"github.com/stretchr/testify/assert"
)

`)

	fuzzScript.WriteString(`#!/bin/sh
set -ex

TIME=5s

`)

	// samples, err := os.ReadDir(pathSamples)
	// errcheck(err)

	samples := []string{
		"add2.go",
		"add.go",
		"assign.go",
		"define.go",
		"func.go",
		"func2.go",
		"if2.go",
		"if3.go",
		"if.go",
		"nest.go",
		"return.go",
		"struct.go",
		"var.go",
	}

	inputSampleMap := make(map[string]string)
	inputSpecMap := make(map[string]string)
	inputMap := make(map[string]string)
	outputLenMap := make(map[string]int)

	for _, name := range []string{"assign", "if3", "return", "define", "var"} {
		inputSampleMap[name] = "uint8(128)"
		inputSpecMap[name] = "a uint8"
		inputMap[name] = "a"
		outputLenMap[name] = 1
	}
	for _, name := range []string{"add2", "add", "if2", "if", "func", "func2", "struct"} {
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

		pkg, err := ast.TranslateFile(samplePath, sampleContent)
		errcheck(err)

		var translateStr strings.Builder
		p := ast.NewPrinter(&translateStr, ast.PrinterOpts{GoCompat: true})
		p.Package(&pkg)

		sampleContentTranslate := translateStr.String()
		sampleContentTranslate = suffixGlobals(sampleContentTranslate, "translate")

		var fuzzTestStr strings.Builder
		err = tmplFuzzTest.Execute(&fuzzTestStr, FuzzTestData{
			NameCapital: strcase.ToCamel(sampleName),
			Name:        sampleName,
			PassCapital: "Translate",
			Pass:        "translate",
			InputSample: inputSampleMap[sampleName],
			InputSpec:   inputSpecMap[sampleName],
			Input:       inputMap[sampleName],
			OutputLen:   outputLenMap[sampleName],
		})
		errcheck(err)

		sampleContent = strings.ReplaceAll(sampleContent, "package main\n", "")
		fuzzSamples.File.WriteString(sampleContent)
		fuzzSamplesTranslate.WriteString(sampleContentTranslate)
		fuzzSamplesTranslate.WriteString("\n")
		fuzzTest.WriteString(fuzzTestStr.String())
		fuzzTest.WriteString("\n")
		fuzzScript.WriteString(fmt.Sprintf(
			"go test ./fuzz -fuzztime ${TIME} -fuzz=\"^FuzzTranslate%v$\"\n",
			strcase.ToCamel(sampleName),
		))
	}
}
