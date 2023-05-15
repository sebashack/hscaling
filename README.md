```
- ST0263, Project 2
Students:
- Sebastian Pulido Gomez, spulido1@eafit.edu.co
- Danilo De Jesus Toro Echeverri, djtoroe@eafit.edu.co

Professor:
- Edwin Nelson Montoya Munera, emontoya@eafit.edu.co
```

# Autosaling project

## 1) Description

### 1.1) Accomplished requirements

Our autoscaling implementation has the following accomplishments:

- A system with a monitor server.
- A system with a monitor client.
- A monitor server that receives metrics from monitor clients via gRPC.
- A monitor client that sends metrics to monitor server via gRPC.
- A monitor client that simulates metrics via a poison distribution.
- A monitor server that can create EC2 instances equipped with a monitor client and a configuration file.
- A monitor server that can send ICMP PING requests to launched instances to check their presence.
- A monitor server that can relaunch dead instances if necessary.
- A monitor server that can scale instances up or down depending on policies such as http-load or cpu-load.
- A monitor server with multiple configurable parameters via a config file.


## 2) Architecture

![scaling-arch](./assets/scaling-arch.png)


This diagram depicts three main components in our system:

- *Monitor service (green box)*: It's main function is to receive metrics from monitor clients which are used to determine
  whether the service should be scaled up (running more instances) or down (terminating some instances). Metrics are received
  via gRPC and storage in a [Sqlite](https://www.sqlite.org/fileformat.html) file where each one of the created instances
  is associated with a record of metrics. The monitor service also dispatches ICMP PING requests to determine whether an
  instance is alive or not. The timeout, count and frequency of PINGs is configurable. If an instance is determined to be
  dead, a new one will be relaunched only if the current count is less than the minimum quota specified in the configuration.
  Finally, the AWS module in the monitor server is capable of launching the new instance together with a script that is
  lunched when the instance is booted. This way, every instance is initialized with a proper configuration file that has
  all of the necessary information for the client monitor to reach the server.
- *Monitor client (red box)*: It's only function is to send metric to the monitor server via gRPC. There are two types of
  metrics, namely, http-load and cpu-load and both range from 0 to 100. Both types of metrics are simulated via a Poisson
  distrbution in order to prevent abrupt changes.
- *AWS cloud (blue box)*: It's the cloud where our autoscaling system operates. The monitor server is capable of running and
  terminating instances via the AWS API and proper credentials that are provided in the configuration file.

## 3) Development environment

### 3.1) Operating system

This lab project was developed and tested in Ubuntu 22.04.

### 3.2) Programming languages

*Monitor service*

The monitor service was implemented with the [Haskell](https://www.haskell.org) programming language with compiler `ghc-8.10.7`.
All of the dependencies together with their versions are specified in the [package.yaml](hsAutoscaling/package.yaml) file
in the `hsAutoscaling` subproject.

*Monitor client*

The monitor client was implemented with the [Haskell](https://www.haskell.org) programming language with compiler `ghc-8.10.7`.
All of the dependencies together with their versions are specified in the [package.yaml](hsMonitor/package.yaml) file
in the `hsMonitor` subproject.

### 3.3) Dir tree

```
hscaling
├── build-release.sh
├── first-time-install.sh
├── hsAutoscaling
│   ├── app
│   │   └── Main.hs
│   ├── asg-config.yaml
│   ├── CHANGELOG.md
│   ├── db
│   │   ├── create-db.sh
│   │   └── schema.sqlite
│   ├── gen-buffs.sh
│   ├── hsAutoscaling.cabal
│   ├── LICENSE
│   ├── package.yaml
│   ├── README.md
│   ├── Setup.hs
│   ├── src
│   │   ├── AutoScalingGroup
│   │   │   ├── App.hs
│   │   │   ├── AWS.hs
│   │   │   ├── CRUD.hs
│   │   │   ├── Env.hs
│   │   │   ├── Ping.hs
│   │   │   └── Scaling.hs
│   │   └── Grpc
│   │       ├── Protobuf
│   │       │   └── Monitor.hs
│   │       └── Server.hs
│   ├── stack.yaml
│   ├── stack.yaml.lock
│   └── test
│       └── Spec.hs
├── hsMonitor
│   ├── app
│   │   └── Main.hs
│   ├── CHANGELOG.md
│   ├── gen-buffs.sh
│   ├── hsMonitor.cabal
│   ├── LICENSE
│   ├── package.yaml
│   ├── README.md
│   ├── Setup.hs
│   ├── src
│   │   ├── Env.hs
│   │   ├── Grpc
│   │   │   ├── Client.hs
│   │   │   └── Protobuf
│   │   │       └── Monitor.hs
│   │   └── MetricGen.hs
│   ├── stack.yaml
│   ├── stack.yaml.lock
│   └── test
│       └── Spec.hs
├── protos
│   └── grpc
│       └── protobuf
│           └── monitor.proto
├── README.md
└── style.sh
```


### 3.4) Build process

#### 3.4.1) Build process

To install the dependencies necessary to build and work on this project execute the [first-time-install.sh](./first-time-install.sh)
script at the root of this repository:

```
./first-time-install.sh
```

#### 3.4.2) Sqlite db

To create an sqlite database compatible with this project, run the [hsAutoscaling/db/create-db.sh](./hsAutoscaling/db/create-db.sh)
script at `hsAutoscaling/db`. The script receives the name of the file:

```
cd hsAutoscaling/db
./create-db.sh monitor.db
```

#### 3.4.3) hsAutoscaling

`hsAutoscaling` is the project where the monitor service and autoscaling group is implemented. All of its dependencies
are managed with the cargo tool already installed with `first-time-install.sh`.

To build the project run

```
cd hsAutoscaling
stack build
```
