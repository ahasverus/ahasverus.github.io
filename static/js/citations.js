fetch("/data/citations.json")
  .then(response => response.json())
  .then(data => {

    Chart.defaults.color = "#D8DEE9";
    Chart.defaults.borderColor = "#434C5E";
    Chart.defaults.font.family = "Inter, sans-serif";

    new Chart(document.getElementById("citationsChart"), {
      type: "bar",

      data: {
        labels: data.map(d => d.year),
        datasets: [{
          label: "Citations",
          data: data.map(d => d.citations),
          backgroundColor: "rgba(136, 192, 208, 0.85)",
          borderRadius: 0,
          borderSkipped: false,
          hoverBackgroundColor: "#81A1C1",
          hoverBorderWidth: 1
        }]
      },

      options: {
        responsive: true,
        maintainAspectRatio: false,

        plugins: {

          legend: {
            display: false
          },

          tooltip: {
            backgroundColor: "#3B4252",
            titleColor: "#ECEFF4",
            bodyColor: "#ECEFF4",
            borderColor: "#4C566A",
            borderWidth: 1,

            callbacks: {
              title: ctx => `${ctx[0].label}`,
              label: ctx => ` ${ctx.raw} citations`
            }
          }
        },

        scales: {
            
          x: {
            grid: {
                display: false
            },
            ticks: {
                color: "#D8DEE9"
            },
            border: {
                display: false
            }
          },
          y: {
            display: false
          }
        }
      }
    });
  })
  .catch(error => console.error(error));
