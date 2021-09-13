# colors-of-the-stars
Sentiment and other natural language analysis of my first short story collection, "Colors of the Stars"


For a browser-readable version of the appendix as it appears in the print version of the book (with a high-level explanation, as well as the code and plots), go [here](https://kollmi.github.io/appendix.html)



If you're looking for the code only, here's a short directory to the repository:

The main script is titled [functions.R](https://github.com/kollmi/colors-of-the-stars/blob/main/functions.R). For each story (in the [stories](https://github.com/kollmi/colors-of-the-stars/tree/main/stories) folder), three graphics are produced:
- A **wordcloud** with the most frequent words in the story being the largest
- A **sentiment trend** curve that plots the sentiment of each sentence (1 being positive, 0 being neutral, -1 being negative). A couple sentences at each extreme is denoted as well.
- A **smoothed sentiment trend** curve that implements local regression (LOESS) to smooth out the standard sentiment curve.

Each story's plots can be found in the [plots](https://github.com/kollmi/colors-of-the-stars/tree/main/plots) folder.
