rem mvn install:install-file -Dfile=ifxjdbc.jar -DartifactId=ifxjdbc -Dversion=1.0 -DgroupId=ifxjdbc -Dpackaging=jar -DlocalRepositoryPath=maven_repository -DgeneratePom=true
rem mvn install:install-file -Dfile=ifxjdbcx.jar -DartifactId=ifxjdbcx -Dversion=1.0 -DgroupId=ifxjdbc -Dpackaging=jar -DlocalRepositoryPath=maven_repository -DgeneratePom=true

mvn deploy:deploy-file -DgroupId=ifxjdbc -DartifactId=ifxjdbc  -Dversion=1.0 -Dpackaging=jar -Dfile=ifxjdbc.jar  -Durl=file:maven_repository
mvn deploy:deploy-file -DgroupId=ifxjdbc -DartifactId=ifxjdbcx -Dversion=1.0 -Dpackaging=jar -Dfile=ifxjdbcx.jar -Durl=file:maven_repository

lein pom
mvn -U dependency:resolve

lein deps

mvn dependency:copy-dependencies
