[![Build Status](https://travis-ci.com/iGOEGGO/dipl.svg?branch=main)](https://travis-ci.com/iGOEGGO/dipl)
[![CircleCI](https://circleci.com/gh/iGOEGGO/dipl.svg?style=svg&circle-token=83b18c34b6f365dc697f4549d8b7ef07b51f54ce)](https://app.circleci.com/pipelines/github/iGOEGGO/dipl)


# dipl

## Umsetzung 

Dieses Package wurde zur Verwendung im igoeggo geschrieben und dient damit zum Einlesen und Säubern sowie dem anschließenden weitergeben von Datensätzen. 



## Deployment 

### Lokales Deployment 

Für das lokale Deployment reicht es aus im root-Verzeichnis folgenden R-Befehl auszuführen: 

```
devtools::load_all()
```

Damit werden die entsprechenden Funktionen des Packages so lange zur Verfügung gestellt, bis man die R-Session beendet. 



## Testing

### Build-in-functions

Das erste, dass beim Testing passiert ist, dass das Package mit der build-in-function `check` von [R](https://www.rdocumentation.org/packages/devtools/versions/2.3.2/topics/check) überprüft wird. 

Ist dieser Schritt erfolgreich werden das Installieren sowie die einzelnen Funktionalitäten des Packages getestet. 