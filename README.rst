This is a personal set of helper functions used to create a web API service.

We assume that the service is intended to run in a container on a Kubernetes
cluster, but it\'s likely that these routines would be useful for other
services.

We provide command-line options for use with optparse-applicative,
Prometheus instrumentation, and some basic logging.
