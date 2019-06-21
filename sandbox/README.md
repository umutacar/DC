# Overview

This document contains a trace of commands that we (Alex and I) to play with docker.  Our goal was to create a Docker container for compiling MTL.

# Trace


```
bash-3.2$ docker
```

Look for an ocaml compatible docker image
 
```
bash-3.2$ docker run ocaml/opam2:centos
Unable to find image 'ocaml/opam2:centos' locally
centos: Pulling from ocaml/opam2
[1BDigest: sha256:8a0660cee79ae08ed36634c651ed648310f14ea0f3bee4e71f8b641af2c05953
Status: Downloaded newer image for ocaml/opam2:centos
```

```
bash-3.2$ docker info
bash-3.2$ docker ps
bash-3.2$ docker image ls
REPOSITORY          TAG                 IMAGE ID            CREATED             SIZE
<none>              <none>              9a510f19198a        2 hours ago         569MB
ubuntu              18.10               6f73120de66c        41 hours ago        67.3MB
ocaml/opam2         centos              6a27f30b8524        2 days ago          2.68GB
```

Run the docker image.  You can run an image without making a container.  Running makes the container and destroys it, at least logically, though they are kept around for use in the future if needed.  

In the example below `ocaml -version` is a command to run on the container.

```
bash-3.2$ docker run ocaml/opam2:centos ocaml -version

bash-3.2$ docker run ocaml/opam2:centos opam show core

bash-3.2$ docker run ocaml/opam2:centos opam show getopt
```

Now let's install core on the image

```
bash-3.2$ docker run ocaml/opam2:centos opam install --dry-run core
The following actions will be simulated:...

```

We can see all the containers that we have been building

```
bash-3.2$ docker ps --all
CONTAINER ID        IMAGE                COMMAND                  CREATED             STATUS                      PORTS               NAMES
```


# Dockerfile

At this point, we decided to create our own dockerfile as follows.

```
FROM ocaml/opam2:centos

RUN opam install core getopt menhir re2
```

The `opam` commants install various packages.

We then built an image by, default file is `Dockerfile`

```
bash-3.2$ docker build -t mtl:lambda .
...
```

```
bash-3.2$ docker image ls
...
```

Now we can tag our docker image with "umutacar" which places it into Umut's dockerhub.

``` 
bash-3.2$ docker tag mtl:lambda umutacar/mtl:lambda
bash-3.2$ docker image ls
REPOSITORY          TAG                 IMAGE ID            CREATED              SIZE
mtl                 lambda              7f676ab62096        About a minute ago   3.58GB
umutacar/mtl        lambda              7f676ab62096        About a minute ago   3.58GB
<none>              <none>              9a510f19198a        2 hours ago          569MB
ubuntu              18.10               6f73120de66c        41 hours ago         67.3MB
ocaml/opam2         centos              6a27f30b8524        2 days ago           2.68GB
```

We push the docker image to dockerhub

```
bash-3.2$ docker push umutacar/mtl:lambda
```

Login required

```
bash-3.2$ docker login
```


# Next steps
Upload MTL sources to the container.
