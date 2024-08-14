<script lang="ts">
  import { onMount } from "svelte";
  import BackButton from '$lib/components/BackButton.svelte';
  import ExplanationButton from '$lib/components/ExplanationButton.svelte';
  import CategorySelector from '$lib/components/CategorySelector.svelte';
  import NetworkVisualization from '$lib/components/NetworkVisualization.svelte';
  import PresidentSelector from '$lib/components/PresidentSelector.svelte';
  import { selectedCategories, avisRange, networkVisible } from '$lib/stores';

  // Import JSON data
  import mesographNodes from '../../static/data/mesograph_nodes.json';
  import mesographEdges from '../../static/data/mesograph_edgelist.json';
  import maxMinParPresident from '../../static/data/max_min_par_president.json';

  // Explicitly define the type for allCategories
  let allCategories: string[] = [];

  onMount(() => {
    try {
      allCategories = [...new Set(mesographNodes.map((node: { Categorie: string }) => node.Categorie))];
      selectedCategories.set(allCategories);
    } catch (error) {
      console.error("Error loading data:", error);
    }
  });

  function handleCategoryUpdate(event: CustomEvent<{ selectedCategories: string[] }>) {
    selectedCategories.set(event.detail.selectedCategories);
  }

  function handlePresidentUpdate(event: CustomEvent<{ avisRange: [number, number] }>) {
    avisRange.set(event.detail.avisRange);
  }
</script>

<BackButton url="https://leomaurice.github.io/CCNE/" />
<ExplanationButton />

<h1>Visualisation du Réseau de Citations du CCNE</h1>

<div>
  <label for="toggleNetwork">Réseau:</label>
  <input type="checkbox" id="toggleNetwork" bind:checked="{$networkVisible}">
</div>

<div>
  <label for="avisRange">Sélectionner la plage des avis à afficher:</label>
  <input type="range" id="avisRange" min="1" max="144" bind:value="{$avisRange}">
</div>

<PresidentSelector {maxMinParPresident} on:update={handlePresidentUpdate} />

<CategorySelector {allCategories} on:update={handleCategoryUpdate} />

{#if $networkVisible}
  <NetworkVisualization
    {selectedCategories}
    {avisRange}
    {mesographNodes}
    {mesographEdges}
  />
{/if}