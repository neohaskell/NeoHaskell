# NeoHaskell event attribution

These terms describe how commands and their resulting events retain attribution across background work.

## Language

**Initiating actor.**
The user whose verified request began a chain of work. Historical attribution carries no permission to act.

**Correlation chain.**
A group of commands and events that share the identity of their initiating operation. Autonomous work can begin a chain without a user.

**Immediate cause.**
The event that directly triggered subsequent command execution. Events emitted together by that command share the same immediate cause.
