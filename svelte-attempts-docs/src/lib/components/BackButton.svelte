<script>
  export let url;
</script>

<button on:click="{() => window.location.href = url}">
  Retour aux différents choix
</button>

<style>
  button{
    position: fixed;
    top: 10px;
    right: 10px;
    z-index: 1000;
  }
</style>