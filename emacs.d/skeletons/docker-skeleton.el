(define-skeleton docker-skeleton
  "" ""
  "FROM ubuntu\n"
  "\n"
  "ENV DEBIAN_FRONTEND noninteractive\n"
  "\n"
  "RUN apt-get -y update && apt-get -y install " _ "\n"
  "\n"
  "RUN adduser --gecos '' developer\n"
  "USER developer\n"
  )

(define-auto-insert "/Dockerfile$" 'docker-skeleton)
