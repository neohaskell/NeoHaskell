# How to write collection type modules

Collections are a key part of functional programming and the fact that
their API should be somewhat standard and compatible is a key part
of the developer experience behind NeoHaskell.

All collection types should define the following functions in order to
maintain a cohesion between using them especially when one wants to
migrate from one to another (e.g. from an Array to a Stream).

The collection module will be imported qualified so the functions should
never include the name of the type as a prefix or suffix.

## Constructors

- `empty`: Constructs an empty collection

- `wrap`: Constructs a collection of one element

## Basic functions

- `isEmpty`:
- `length`:
- `append`: Appends an element to the end of the collection
- `flatten`:
- `map`: Applies a function to each of the elements
- `takeIf`: Akin to the common filter, takes element if the predicate is true
- `reduce`:
- `reduceFromRight`: Reduce, but starting from the end of the collection
- `dropIf`: The opposite of takeIf, drops elements if the predicate is false
- `mapMaybe`: Like map, but dropping the element if the function returns Nothing
- `dropNones`: Filters out the Nothings out of a collection

## Transformations

- `reverse`:
- `intersperse`: Puts an element between each element

## Subarrays

- `slice`:
- `take`: Takes the first N elements of the collection
- `drop`: Drops the first N elements of the collection
- `splitAt`:

## Searching

- `contains`:
- `doesNotContain`:
- `findIndex`:

## Accessing elements

- `get`: Gets a specific element by index, returning a Maybe
- `first`:
- `last`:
- `allButFirst`:
- `allButLast`:

## Zipping and unzipping

- `pairWith`:
- `combineWith`:
- `splitPairs`:

## Special reduces

- `allTrue`: Reduces the collection, ensuring that all elements are true
- `anyTrue`: Same but only for any
- `anySatisfy`: Like the above but for predicates
- `allSatisfy`:

## Set operations

- `union`:
- `intersect`:
- `difference`:

## Additional operations

- `sort`:
- `sortBy`:
- `deduplicate`:
