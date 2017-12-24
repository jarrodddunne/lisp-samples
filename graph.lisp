; Jarrod Dunne
; Graph package

; Graph structure with (set hashtable)
(defstruct graph
  nodes
  edges)

; Returns a new graph with empty node-set, empty hash-table
(defun new-graph ()
  (make-graph
   :nodes '()
   :edges (make-hash-table)))

; Adds a node to the node-set
(defun add-node (graph node)
  (setf
   (graph-nodes graph)
   (adjoin
    node
    (graph-nodes graph))))

; Adds an edge to the graph
(defun add-edge (graph node1 node2)
  (setf
   (gethash
    node1
    (graph-edges graph))
   (adjoin
    node2
    (gethash
     node1
     (graph-edges graph)))))

; Adds a undirected edge to the graph
(defun add-undir-edge (graph node1 node2)
  (progn
    (add-edge graph node1 node2)
    (add-edge graph node2 node1)))

; Gets the neighbors of a particular node
(defun get-neighbors (graph node)
  (gethash
   node
   (graph-edges graph)))

; Prints a string representation of the graph
(defun print-graph (graph)
  (mapc
   #'(lambda (node)
       (format t "~A : ~A~%" node (get-neighbors graph node)))
   (graph-nodes graph)))
