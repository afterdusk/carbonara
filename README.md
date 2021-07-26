# carbonara

Discord pomodoro bot and terrible OTP application.

## Commands

### `cb!start`
Starts a pomodoro session with **25** minutes work and **5, 5, 5, 15** minutes break cycles. Work/break pings will be sent to the channel where the command was run in. No VC support yet.

### `cb!stop`
Stops the ongoing pomodoro session.

### `cb!join`
Adds the command runner to a list of pomodoro followers. You'll get @mentioned for each work/break ping.

### `cb!leave`
Removes the command runner from pomodoro followers, and future pings will not mention you.

## Prerequisites
- Erlang
- Rebar3
- Env variables:
    - `DISCORD_TOKEN`

## Usage

### Build
```shell
$ rebar3 compile
```

### Run
```shell
$ rebar3 shell
```


## Technical TODOs
- Spawn a supervised process for each pom session, so that the app can support multiple pom sessions across servers
- Typing
- Docs
- Tests
- Docker
- Better handling of env variables (`DISCORD_TOKEN`)

## Features TODO
- VC pings
- Customizable time
- Calculate time of next work session/break
- Command and/or GUI to check remaining time
- Slash commands

## Attribution
A lot of the code using the Discord library came from [mikeyhc/yshtola](https://github.com/mikeyhc/yshtola).
