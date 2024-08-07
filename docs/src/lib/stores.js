import { writable } from 'svelte/store';

export const selectedCategories = writable([]);
export const avisRange = writable([125, 144]);
export const networkVisible = writable(true);