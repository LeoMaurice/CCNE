<script lang="ts">
  import { createEventDispatcher } from 'svelte';
  import { writable, type Writable } from 'svelte/store';
  
  export let allCategories: string[] = [];
  export let selectedCategories: Writable<string[]> = writable<string[]>([]);

  const dispatch = createEventDispatcher();

  function toggleCategories() {
    selectedCategories.update((selected: string[]) => {
      if (selected.length === 0) {
        return allCategories;
      } else {
        return [];
      }
    });
  }

  $: selectedCategories.subscribe(value => dispatch('update', { selectedCategories: value }));
</script>

<label for="categories">Choisir les catégories à afficher:</label>
<div id="categories">
  {#each allCategories as category}
    <input type="checkbox" value="{category}" bind:group="{selectedCategories}"> {category}
  {/each}
</div>
<button on:click="{toggleCategories}">Décocher/Recocher toutes les catégories</button>