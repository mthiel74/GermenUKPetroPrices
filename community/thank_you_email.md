# Thank-you note to the Tankerkönig team

Address: **info@tankerkoenig.de** (and copy `onboarding@tankerkoenig.de`
if you have a contact there from your bulk-archive registration).

Send this after the Wolfram Community post is live so you can include
the URL.

---

**Subject:** Danke für die Tankerkönig-Daten — Wolfram-Community-Artikel zur 12-Uhr-Regel

Liebes Tankerkönig-Team,

ich möchte mich auf diesem Weg ganz herzlich für die Arbeit bedanken,
die Sie seit Jahren in die Tankerkönig-API und das historische
CSV-Archiv stecken. Die Kombination aus Echtzeit-REST-API, durchdachtem
Lizenzmodell und kostenlosem Zugang zum Forschungsarchiv ist in Europa
sehr selten — und für unabhängige Forschung und Lehre Gold wert.

Ich habe gerade einen empirischen Beitrag auf der Wolfram Community
veröffentlicht, der die Auswirkungen der 12-Uhr-Regel (1. April 2026)
auf den lokalen Tagesgang der Dieselpreise in Paderborn analysiert,
mit einem britischen Kontrollcluster in Aberdeen. Der Beitrag enthält
unter anderem eine kriging-basierte Animation des bundesweiten
Dieselpreisfelds über November 2019, gebaut aus den 14 500 Stationen
des öffentlichen `gustavz/tankerkoenig_dataset`-Mirrors. Praktisch
jede Visualisierung in dem Artikel existiert nur, weil Sie diese
Daten zur Verfügung stellen.

Link zum Artikel: **[bitte einfügen]**
Repository und Code: https://github.com/mthiel74/GermanUKPetroPrices

Ein Hinweis in eigener Sache: bei einem unbeobachteten automatisierten
Skript habe ich Mitte Mai für ein paar Stunden die 1-Anfrage-pro-Minute-
Empfehlung Ihrer API überschritten (etwa 60 Anfragen in einer Minute,
bevor mir auffiel, was passierte). Falls dadurch in Ihren Logs eine
auffällige Spitze auf den Schlüssel `37ed1238-…` erscheint: das war ich,
es tut mir leid, ich verspreche besseres Verhalten. Mein Skript hat
inzwischen einen `--i-have-read-the-rules`-Schutz und eine
60-Sekunden-Pause zwischen den Anfragen fest eingebaut.

Falls Sie einen Forschungs-Key mit höherem Kontingent kennen würden,
würde mich das für eine geplante Folgeanalyse (echtes
landesweit-kriging über das 12-Uhr-Regel-Fenster Sep 2025 – Mai 2026)
sehr interessieren. Aber das ist natürlich nichts, was Sie tun müssen
— ich bin schon mehr als zufrieden mit dem, was öffentlich verfügbar
ist.

Nochmal: ganz herzlichen Dank. Die Qualität, Offenheit und
Verlässlichkeit Ihres Dienstes hat diesen Beitrag erst möglich gemacht.

Mit besten Grüßen
Marco Thiel
University of Aberdeen
m.thiel@abdn.ac.uk

---

## English version (if you'd prefer to send that)

**Subject:** Thank you for the Tankerkönig data — Wolfram Community post on the 12-Uhr-Regel

Dear Tankerkönig team,

I just wanted to write and thank you, sincerely, for the work you put
into the Tankerkönig API and the historical CSV archive. The
combination of a real-time REST API, a thoughtful licence regime, and
free access to the research archive is genuinely rare in Europe —
and an enormous gift to independent researchers and educators.

I have just published an empirical Wolfram Community post studying the
effect of the 12-Uhr-Regel (1 April 2026) on the intraday cycle of
diesel prices, using my own local minute-resolution feed from a
Paderborn 10 km cluster, with an Aberdeen UK control cluster.
The post also includes a kriging-based animation of the country-wide
German diesel-price field through November 2019, built from the
14 500 stations of the public `gustavz/tankerkoenig_dataset` mirror.
Almost every figure in the post exists because you make this data
available.

Link to the post: **[please insert]**
Code + repository: https://github.com/mthiel74/GermanUKPetroPrices

A confession: in mid-May an unattended automated script of mine
exceeded your "one request per minute" guidance for a couple of hours
(roughly 60 requests inside one minute before I noticed). If you see
a suspicious spike on key `37ed1238-…` in your logs, that was me,
and I am sorry. The script now has a hard `--i-have-read-the-rules`
guard and a baked-in 60-second pause between requests; I will not let
that happen again.

If there is a research-tier key with a higher quota that I could
register for, I would be very interested for a planned follow-up
analysis (a true country-wide kriged time-series covering the
12-Uhr-Regel window Sep 2025 – May 2026). That is of course in no way
expected — I'm already extremely happy with what's publicly available.

Once again: thank you. The quality, openness and reliability of your
service is what made this project possible.

With warm regards,
Marco Thiel
University of Aberdeen
m.thiel@abdn.ac.uk
