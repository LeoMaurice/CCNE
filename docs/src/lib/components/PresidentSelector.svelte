<script lang="ts">
  import { createEventDispatcher } from 'svelte';
  import { avisRange } from '$lib/stores';

  export let maxMinParPresident = [];

  const dispatch = createEventDispatcher();

  function setAvisRange(min: number, max: number) {
    avisRange.set([min, max]);
    dispatch('update', { avisRange: [min, max] });
  }
</script>

<h4>Sélectionnez un président pour ajuster le slider avec les avis publiés sous son mandat :</h4>
{#each maxMinParPresident as { president, avis_max, avis_min }, i}
  <button on:click="{() => setAvisRange(avis_min, avis_max)}">{president}</button>
{/each}