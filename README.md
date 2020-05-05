# Das Spinnenweben ('Cobweb') Modell

Diese App implementiert das Cobweb Modell.

Eine genaue Beschreibung des Modells und der Implementierung findet sich in
`beschreibung/Cobweb_Beschreibung.pdf`.

Mögliche Leitfragen für die Experimente mit dem Modell:

1. Welchen Einfluss haben die Achsenabschnitte auf die Preisdynamiken?

2. Was sind die Implikationen unterschiedlicher Ausgangspreise?

3. Wie verändern sich die Preisanpassungsdynamiken bei unterschiedlichen Angebots- und Nachfrageelastizitäten?

4. Was sind die wichtigsten Determinanten der Konvergenz- bzw. Divergenzgeschwindigkeit? Was ist hier die Intuition?


## Technische Hinweise zur Verwendung der App

Um die App zu verwenden gehen Sie folgendermaßen vor:

1. Laden Sie sich die Repository herunter
2. Öffnen Sie das entsprechende R Projektfile `cobweb.Rproj`
3. Öffnen Sie das file `app.R`
4. Klicken Sie oben rechts aus `Run App` (ich empfehle Ihnen dabei über das Drop-Down Menü `Run external` auszuwählen, damit die App im Browser geöffnet wird)

Alternativ können Sie auch den Link im Moodle verwenden, allerdings ist die 
Nutzungszeit hier pro Monat für den Kurs beschränkt und Sie können sich nicht 
den Code ansehen.

Eine dritte Möglichkeit besteht darin, die App lokal zu installieren und über
ihre lokale R-Version zu benutzen. 
Führen Sie dazu folgenden Code aus:

```
shiny::runGitHub(username = "graebnerc", repo = "cobweb")
```

## Hinweise zu notwendigen Paketen und eventuellen Updates

Wenn Sie die App auf Ihrem Computer verwenden wollen müssen Sie bestimmte 
Pakete installiert haben. Zudem sollte Ihre R Version nicht zu alt sein.
Sie können das automatisiert mit dem Skritpt `versionstest_cobweb.R` überprüfen. 
Hier werden eventuell fehlende Pakete automatisch installiert. 
Zudem bekommen Sie einen Hinweis wenn Sie R oder ein Paket updaten sollten.

Wenn Sie R auf Windows updaten müssen gehen Sie folgendermaßen vor:

1. Installieren Sie das Paket `installr`: `install.packages("installr")`
2. Führen Sie folgenden Code aus:

```
library(installr)
updateR()
```

3. Checken Sie ob R-Studio geupdated werden muss indem Sie unter `Hilfe` nach neuen Versionen suchen.

Wenn Sie R auf dem Mac updaten wollen, installieren Sie R einfach neu.



Für eventuelle Fragen melden Sie sich bitte bei Claudius Gräbner
