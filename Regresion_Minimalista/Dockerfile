FROM rhub/r-minimal

LABEL maintainer = "Cristian Heredia cristianvoice@gmail.com"

RUN apk update
RUN apk add --no-cache --update-cache \
    --repository http://nl.alpinelinux.org/alpine/v3.11/main \
    autoconf=2.69-r2 \
    automake=1.16.1-r0 \
    bash tzdata

RUN echo "America/Edmonton" > /etc/timezone

RUN installr -d \
    -t "R-dev file linux-headers libxml2-dev gnutls-dev openssl-dev libx11-dev cairo-dev libxt-dev" \
    -a "libxml2 cairo libx11 font-xfree86-type1" \
    remotes shiny forecast jsonlite ggplot2 htmltools plotly Cairo

RUN rm -rf /var/cache/apk/*

RUN addgroup --system app && adduser --system --ingroup app app
WORKDIR /home/app
COPY app .
RUN chown app:app -R /home/app
USER app

EXPOSE 3838

CMD ["R", "-e", "options(tz='America/Edmonton');shiny::runApp('/home/app', port = 3838, host = '0.0.0.0')"]