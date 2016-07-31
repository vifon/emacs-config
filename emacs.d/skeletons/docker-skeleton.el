(define-skeleton docker-skeleton
  "" ""
  "FROM alpine\n"
  "\n"
  "RUN apk --no-cache add " _ "\n"
  "\n"
  "RUN adduser -D app"
  "USER app\n"
  "WORKDIR /home/app\n"
  "\n"
  "EXPOSE 5000\n"
  "\n"
  "CMD [\"/bin/sh\"]\n"
  )

(define-auto-insert "/Dockerfile$" 'docker-skeleton)
