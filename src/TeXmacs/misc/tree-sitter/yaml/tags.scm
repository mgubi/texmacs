; The outline of YAML (the keys of the first two levels) for TeXmacs (progs/treesit/ts-edit.scm): ours, the
; grammar has no tags.scm.

(document (block_node (block_mapping
  (block_mapping_pair key: (flow_node) @name) @definition.key)))
(document (block_node (block_mapping (block_mapping_pair value: (block_node (block_mapping
  (block_mapping_pair key: (flow_node) @name) @definition.key))))))
