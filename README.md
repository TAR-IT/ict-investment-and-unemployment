<!-- TODO: - (Stand 04.03.2025) -->

# Bachelorarbeit – Politikwissenschaften B.A.
**Titel:** Der Einfluss von ICT-Investitionen auf die Arbeitslosigkeit in verschiedenen Bildungsgruppen in OECD-Ländern\
**Autor:** Tobias A. Rau\
**Matrikelnummer:** 6619097\
**Universität:** Goethe-Universität Frankfurt\
**Fachbereich:** Fachbereich 03 – Geisteswissenschaften\
**Studiengang:** Politikwissenschaften B.A. (Nebenfach Soziologie)\
**Abgabedatum:** 25.03.2025\
**Projektarchivierung:** [GitHub Repository](https://github.com/TAR-IT/powi-bachelorthesis)

---
---

## Projektbeschreibung

Diese Bachelorarbeit untersucht den Zusammenhang zwischen **Investitionen in Informations- und Kommunikationstechnologie (ICT)** und der **Arbeitslosigkeit in verschiedenen Bildungsniveaus** in OECD-Ländern. Die Analyse basiert auf **Paneldaten (2005–2022)** und verwendet **Fixed Effects-Modelle** mit Interaktionseffekten zur empirischen Untersuchung der Hypothesen.

### Forschungsfrage

> *Wie beeinflussen nationale Investitionen in Informations- und Kommunikationstechnologien die Arbeitslosenquoten verschiedener Bildungsniveaus in Wohlfahrtsstaaten?*

### Hypothesen

- **H1:** Länder, in denen verstärkt in Informations- und Kommunikationstechnologien investiert wird, weisen eine geringere Arbeitslosenquote unter hochqualifizierten Arbeitskräften auf.
- **H2:** In Ländern mit hohen ICT-Investitionen verlagert sich die Arbeitslosigkeit auf niedrigqualifizierte Arbeitskräfte.
- **H3:** Der Typ des Wohlfahrtsstaates hat Einfluss auf die Polarisierung des Arbeitsmarktes. Länder mit stark entwickelten wohlfahrtsstaatlichen Systemen und flexiblen Arbeitsmarktstrukturen zeigen eine geringere Polarisierung auf.

### Daten & Methodik

#### **Datenquellen**

- **OECD-Paneldaten** zu 35 OECD- und 
ausgewählte Nicht-OECD-Länder von 2005–2022 (Arbeitslosenquoten, ICT-Investitionen, BIP, Gewerkschaftsdichte, Tertiärer Bildungsanteil, Regulierungsstrenge des Arbeitsmarktes)

## Projektstruktur

### **R-Codebook** (`R/script.R`)

- Enthält alle relevanten Datenverarbeitungsschritte
- Definition der Variablen und Transformationen
- Beschreibung der Datenquellen
- Speichern und Laden der bereinigten Daten
- Durchführung der statistischen Analysen

### **Datensätze** (`R/data/`)

- Enthält alle in der Analyse genutzen Datensätze
- Die Datensätze enthalten ungefilterte Rohdaten
- Die Filterung überfolgt durch den Code im R-Codebook

## Anforderungen

### Genutzte Technologien

- [R programming language](https://www.r-project.org/)
    - R wurde für die Paneldatenanalyse genutzt.
    - Erforderliche R-Pakete: `ggplot2`, `zoo`, `plm`, `dplyr`, `modelsummary`, `knitr`, `kableExtra`
- [RStudio](https://posit.co/download/rstudio-desktop/)
    - RStudio wurde als interaktive Entwicklerumgebung für R genutzt.
- [LaTeX](https://www.latex-project.org/)
    - LaTeX wurde zur Erstellung des PDF-Dokumentes genutzt.

### Installation/Reproduzierung
1. Repository klonen/herunterladen und entpacken.
2. Die Datei `R/Data.Rproj` ausführen um das Projekt in RStudio zu öffnen.
3. `R/script.R` in RStudio ausführen.
4. `TeX/main.tex` per Recipe "pdflatex -> bibtex  -> pdflatex" kompilieren.