import { writable } from 'svelte/store';

// Use a generic type for selectedCategories
export const selectedCategories = writable<string[]>([]);
export const avisRange = writable<[number, number]>([125, 144]);
export const networkVisible = writable<boolean>(true);