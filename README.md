# l3c-problems

Clarifications needed:

* Problem 1
    * Are the trees defined as normal binary search trees? That is, is the left
      child of a node the subtree containing all elements less than it, and
      the right child the subtree containing all elements greater than it? It
      specifies that "elements appear in order," but not which kind of tree
      traversal is used to define the order.
    * Are the elements in the trees unique, as in the traditional definition of
      a binary search tree?
* Problem 2
    * Clarification on definition of "slices"
        * Why would "1.1.2.1" not be a valid IPv4 address? It seems that "1121"
          is a slice of the original String "11211", so why would it not be
          valid?
        * Why would "2.5.5.0", "2.5.50.5" not be valid IPv4 addresses sliced
          from "25505011535"?
        * Both examples would make sense if slices were not considered, and
          "1.2.1.1" in the first example was a typo, but this is doubtful
    * Should the output list of IPv4 addresses be unique?
