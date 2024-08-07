<script>
  function showExplanation() {
    alert("Explication de l'application ...");
  }
</script>

<button on:click="{showExplanation}">
  Explication de l'application
</button>

<style>
  button{
    position: fixed;
    top: 50px;
    right: 10px;
    z-index: 1000;
  }
</style>