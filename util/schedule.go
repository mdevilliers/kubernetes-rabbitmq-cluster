package util

import (
	"time"
)

func Schedule(what func(), delay time.Duration) {
	func() {
		time.Sleep(delay)
		what()
	}()
}

func ScheduleInNewGoRoutine(what func(), delay time.Duration) {
	go Schedule(what, delay)
}
