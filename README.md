# elevator

Simulating an elevator control system.

The elevator makes stops at requested floors in the direction it's going.  It turns and goes down to the ground floor if it reaches the top or if there are no more requests above the current floor. The elvator stays on the ground floor when there are no requests.

To build and run with docker:

```
# docker build . -t elevator
# docker run
```
