<script>
  import { onMount } from "svelte";
  let Network;

  if (typeof window !== 'undefined') {
    Network = require('vis-network').Network;
  }

  export let selectedCategories;
  export let avisRange;
  export let mesographNodes = [];
  export let mesographEdges = [];
  
  const categoryColors = {
    "Auteurs": "#6CC7B3",
    "Autorités": "#285291",
    "CCNE": "#9D3A5E",
    "Comité d'éthique": "#579125",
    "Comparaison pays": "#0F0F5C",
    "Etat": "#91188F",
    "Forums": "#0E405C",
    "Loi": "#915B11",
    "Org Internationales": "#4F2B91",
    "Presse": "#91181E",
    "Science, littérature": "#0B5C2D",
    "Société": "#BD6345",
    "Avis étudié": "#4269A0"
  };

  let networkContainer;

  onMount(() => {
    if (typeof window !== 'undefined') {
      renderNetwork();
    }
  });

  $: if (selectedCategories && avisRange && typeof window !== 'undefined') {
    renderNetwork();
  }

  function calculateNodeDegree(edges, nodes) {
    const degreeMap = new Map();
    nodes.forEach(node => {
      degreeMap.set(node.name, 0);
    });
    edges.forEach(edge => {
      degreeMap.set(edge.from, degreeMap.get(edge.from) + 1);
      degreeMap.set(edge.to, degreeMap.get(edge.to) + 1);
    });
    return degreeMap;
  }

  function renderNetwork() {
    const selectedCategoriesValue = $selectedCategories;
    const avisRangeValue = $avisRange;

    const filteredNodes = mesographNodes.filter(
      node => selectedCategoriesValue.includes(node.Categorie)
    );

    const filteredEdges = mesographEdges.filter(
      edge => selectedCategoriesValue.includes(edge.Categorie) &&
              (edge.from >= avisRangeValue[0] && edge.from <= avisRangeValue[1] ||
               edge.to >= avisRangeValue[0] && edge.to <= avisRangeValue[1])
    );

    const linkedNodes = new Set(
      filteredEdges.flatMap(edge => [edge.from, edge.to])
    );

    const displayNodes = filteredNodes.filter(node => linkedNodes.has(node.name));

    const nodeDegrees = calculateNodeDegree(filteredEdges, displayNodes);

    const nodes = displayNodes.map(node => ({
      id: node.name,
      label: node.name,
      group: node.Categorie,
      value: nodeDegrees.get(node.name) || 1,
      color: categoryColors[node.Categorie]
    }));

    const edges = filteredEdges.map(edge => ({
      from: edge.from,
      to: edge.to
    }));

    const data = { nodes, edges };
    const options = {
      nodes: {
        scaling: {
          min: 10,
          max: 30,
        }
      },
      edges: {
        arrows: {
          to: { enabled: true, scaleFactor: 1 }
        }
      },
      groups: Object.fromEntries(
        Object.entries(categoryColors).map(([group, color]) => [group, { color }])
      )
    };

    const network = new Network(networkContainer, data, options);
  }
</script>

<div bind:this={networkContainer} style="width: 100%; height: 800px;"></div>