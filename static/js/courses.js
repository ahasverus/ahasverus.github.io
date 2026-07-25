const events = [
  { start: 2015, end: 2015, label: "Bibliography management" },
  { start: 2016, end: 2016, label: "The Markdown syntax" },
  { start: 2016, end: 2016, label: "R Graphics" },
  { start: 2019, end: 2019, label: "Data acquisition" },
  { start: 2019, end: 2020, label: "Spatial data with sf" },
  { start: 2019, end: 2026, label: "An introduction to git" },
  { start: 2019, end: 2026, label: "Reproducible research" },
  { start: 2019, end: 2026, label: "Building an R package" },
  { start: 2020, end: 2026, label: "Docker for R users" },
  { start: 2021, end: 2026, label: "An introduction to renv" },
  { start: 2022, end: 2026, label: "Corpus construction" },
  { start: 2024, end: 2026, label: "Retrieving full texts" },
  { start: 2024, end: 2026, label: "Getting biodiversity data" },
  { start: 2024, end: 2026, label: "Cleaning biodiversity data" },
  { start: 2024, end: 2026, label: "Data formats" },
  { start: 2024, end: 2026, label: "Sharing code & tools" },
  { start: 2024, end: 2026, label: "Software paper" },
  { start: 2025, end: 2026, label: "The DESCRIPTION file" },
  { start: 2025, end: 2026, label: "Writing R functions" },
  { start: 2026, end: 2026, label: "An introduction to Quarto" }
];


const width = 900;
const rowHeight = 12;

const margin = {
  top: 30,
  right: 20,
  bottom: 10,
  left: 20
};


// Mesure la largeur du texte
function getTextWidth(text) {
  const canvas = document.createElement("canvas");
  const context = canvas.getContext("2d");

  context.font = "12px sans-serif";

  return context.measureText(text).width;
}


// Calcul position carte
function cardX(d) {
  const cardWidth = getTextWidth(d.label) + 20;
  const center = (x(d.start) + x(d.end)) / 2;

  return Math.max(
    margin.left,
    Math.min(
      center - cardWidth / 2,
      width - margin.right - cardWidth
    )
  );
}


const svg = d3.select("#timeline")
  .append("svg")
  .attr(
    "width",
    width
  )
  .attr(
    "height",
    margin.top + events.length * rowHeight + margin.bottom
  )
  .style("overflow", "visible");


const x = d3.scaleLinear()
  .domain([2015, 2026])
  .range([margin.left, width - margin.right]);


// Axe des années
svg.append("g")
  .attr("transform", `translate(0, ${margin.top - 8})`)
  .call(
    d3.axisTop(x)
      .tickValues(d3.range(2015, 2027))
      .tickFormat(d3.format("d"))
      .tickSize(0)
  )
  .call(g => g.select(".domain").remove());


// Groupes événements
const eventGroup = svg.selectAll(".event")
  .data(events)
  .join("g")
  .attr("class", "event")
  .attr(
    "transform",
    (_, i) => `translate(0, ${margin.top + i * rowHeight})`
  );


// Barres
eventGroup.append("rect")
  .attr("class", "event-bar")
  .attr("x", d => x(d.start - 0.15))
  .attr(
    "width",
    d => Math.max(
      8,
      x(d.end + 0.15) - x(d.start - 0.15)
    )
  )
  .attr("height", 8)
  .attr("rx", 0)
  .attr("fill", "rgba(136, 192, 208, 0.85)");


// Cartes
const cards = eventGroup.append("g")
  .attr("class", "timeline-card")
  .style("opacity", 0)
  .style("pointer-events", "none");


// Fond carte
cards.append("rect")
  .attr("x", d => cardX(d))
  .attr("y", 14)
  .attr("width", d => getTextWidth(d.label) + 40)
  .attr("height", 40)
  .attr("rx", 6)
  .attr("fill", "#2e3440")
  .attr("fill-opacity", 1)
  .attr("stroke", "white")
  .attr("stroke-width", 1);


// Texte
cards.append("text")
  .attr(
    "x",
    d => cardX(d) + (getTextWidth(d.label) + 40) / 2
  )
  .attr("y", 40)
  .attr("text-anchor", "middle")
  .attr("fill", "white")
  .attr("font-size", "12px")
  .text(d => d.label);


// Interaction
eventGroup
  .on("mouseover", function() {

    d3.select(this)
      .select(".event-bar")
      .attr("opacity", 0.65);


    d3.select(this)
      .raise()
      .select(".timeline-card")
      .interrupt()
      .transition()
      .duration(50)
      .style("opacity", 1);

  })


  .on("mouseout", function() {

    d3.select(this)
      .select(".event-bar")
      .attr("opacity", 1);


    d3.select(this)
      .select(".timeline-card")
      .interrupt()
      .transition()
      .duration(150)
      .style("opacity", 0);

  });