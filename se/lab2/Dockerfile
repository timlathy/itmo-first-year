FROM java:8-jdk

ENV KOTLIN_VERSION 1.1.4-3

RUN curl -LO https://github.com/JetBrains/kotlin/releases/download/v1.1.4-3/kotlin-compiler-${KOTLIN_VERSION}.zip \
 && unzip kotlin-compiler-${KOTLIN_VERSION}.zip \
 && rm kotlin-compiler-${KOTLIN_VERSION}.zip

ENV PATH "/kotlinc/bin:${PATH}"

ENV GRADLE_VERSION 4.2.1

RUN curl -LO https://services.gradle.org/distributions/gradle-${GRADLE_VERSION}-bin.zip \
 && unzip gradle-${GRADLE_VERSION}-bin.zip \
 && rm gradle-${GRADLE_VERSION}-bin.zip

ENV PATH "/gradle-${GRADLE_VERSION}/bin:${PATH}"
