Feature: tree size
    The size of a tree is defined as the number of nodes in the tree.
    
Scenario: a new tree shall have a size of 0
    Given a new tree
    When asked for the size of tree
    Then a size of 0 must be returned
    
Scenario: adding an object to an empty tree shall give a tree with a size of 1
    Given a new tree 
    And I add one node added to the tree
    When asked for the size of tree
    Then a size of 1 must be returned

Scenario: removing an object from a tree of size n shall give a tree of size n-1
    Given a new tree 
    And I add two nodes to the tree
    When one node is removed
    Then a size of 1 must be returned

Scenario: removing an object from a tree of size 1 shall give a tree of size 0
    Given a new tree 
    And I add one node to the tree
    When one node is removed
    Then a size of 0 must be returned
