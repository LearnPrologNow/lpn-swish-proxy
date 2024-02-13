FROM swipl:stable

RUN apt-get update && apt-get install -y --no-install-recommends \
	locales \
	curl

RUN     sed -i -e 's/# en_GB.UTF-8 UTF-8/en_GB.UTF-8 UTF-8/' /etc/locale.gen && \
        locale-gen
ENV     LC_ALL en_GB.UTF-8
ENV     LANG en_GB.UTF-8
ENV     LANGUAGE en_GB:en

COPY	--from=lpn . /lpn

# Running

copy health.sh health.sh
HEALTHCHECK --interval=30s --timeout=2m --start-period=1m CMD /health.sh

COPY start-lpn.sh start-lpn.sh

ENTRYPOINT ["/start-lpn.sh"]
