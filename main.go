// Assume:
// In the current directory, there are:
// - init.el
// - monotone-theme.el

package main

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
)

const dir = ".eces/monotone"

var home = os.Getenv("HOME")

func move(filename string) error {
	d := filepath.Join(home, dir, filename)
	f, err := os.Open(filename)
	if err != nil {
		return err
	}
	f1, err := os.Create(d)
	if err != nil {
		return err
	}
	_, err = io.Copy(f1, f)
	return err
}

func install() error {
	d := filepath.Join(home, dir)
	err := os.MkdirAll(d, os.ModePerm)
	if err != nil {
		return err
	}
	err = move("init.el")
	if err != nil {
		return err
	}
	err = move("monotone-theme.el")
	return err
}

func main() {
	err := install()
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to install: %v\n", err)
	}
}
