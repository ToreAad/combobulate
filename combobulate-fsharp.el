;;; combobulate-fsharp.el --- fsharp support for combobulate -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Tore Aadland

;; Author: Tore Aadland <tore.aad@gmail.com>
;; Keywords: convenience, tools, languages, fsharp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Combobulate support for F#, built on the `fsharp' tree-sitter grammar
;; from <https://github.com/ionide/tree-sitter-fsharp>.  The grammar is
;; not one Emacs installs on its own; add it to
;; `treesit-language-source-alist' and install it once:
;;
;;   (add-to-list 'treesit-language-source-alist
;;                '(fsharp "https://github.com/ionide/tree-sitter-fsharp"
;;                         "0.3.11" "fsharp/src"))
;;   (treesit-install-language-grammar 'fsharp)
;;
;; `fsharp-mode' is not a tree-sitter mode, but Combobulate does not need
;; it to be: it creates the parser itself once the grammar is installed.
;; Signature files (.fsi) are a separate grammar and are not covered here.
;;
;; F# and OCaml are cousins, but their grammars share no node names, so
;; this is the same ideas as `combobulate-ocaml.el' applied to a rather
;; different tree.  Three shapes of the F# grammar drive most of what
;; follows, and are worth knowing before changing anything here:
;;
;;  - Bindings and statements chain rather than sit side by side.  A run
;;    of `let's in a body is a right-nested tower of
;;    `declaration_expression', each rung holding the rest of the body in
;;    its `in:' field; a run of statements is the same tower built from
;;    `sequential_expression'.  So "the siblings of a statement" is not
;;    "the children of its parent", and the rules below flatten a tower
;;    with a recursive tree-sitter query instead.  Usefully,
;;    `function_or_value_defn' starts *on* its `let' keyword, so the
;;    flattened chain puts point where a reader expects it.
;;
;;    The other half of that is that "the parent of a statement" is not
;;    the construct enclosing it either -- it is the statement above,
;;    whose rung holds it.  Every list of node types that answers "what
;;    encloses point" therefore leaves the two chain nodes out, or the
;;    answer walks up the block one statement at a time: `procedures-defun'
;;    for `C-M-a' and `C-M-h', `procedures-hierarchy' for `C-M-u', and
;;    `procedures-default' for `M-h'.  A rung always starts where its own
;;    statement does, and that statement's node is listed, so nothing
;;    becomes unreachable.
;;
;;  - Type members come in two shapes.  A class with a primary
;;    constructor (`anon_type_defn') wraps every member in its own
;;    `type_extension_elements'; a record, or a `type X with' extension,
;;    puts all of them in a single wrapper.  The members query covers
;;    both by matching the children of every wrapper in the type.
;;
;;  - `tuple_expression' nests to the right and `application_expression'
;;    to the left, so neither is the flat list of children it looks like.
;;
;; Because `:choose parent' only has a parent to choose when the
;; activation rule asked for one, every such rule here carries a
;; `:has-parent'.  And because a procedure is matched against point's
;; whole ancestry -- not just the node at point -- the chain rules use
;; `:position at' so that an ancestor several levels up cannot hijack
;; navigation that a more specific rule further down the list should
;; handle.
;;
;; Two consequences of how a procedure is matched are worth spelling out,
;; because they decide the order of everything in `procedures-sibling'.
;; A procedure is tried against the nodes *at* point first, innermost
;; first, and only then against point's ancestors -- so within a single
;; procedure it is the tree, not the order of the `:activation-nodes'
;; entries, that picks the winner, and two node types that begin at the
;; same character are decided by which is smaller.  Splitting a rule out
;; into a procedure of its own is therefore the only way to give it
;; priority over a smaller node that starts in the same place.
;;
;; One rough edge is left, and it comes from the grammar rather than from
;; these rules: the `|' that introduces a case belongs to the group, not
;; to the case after it, so point sitting exactly on one -- where
;; `back-to-indentation' leaves it -- resolves to the whole group.  From
;; there `C-M-n' steps to the group's first case rather than to the next
;; one.  Skipping `|' as a prefix would fix it, but the same variable
;; tells `M-h' how far the current selection reaches, and a `|' in that
;; set makes the selection stop growing.

;;; Code:

(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-setup)
(require 'combobulate-manipulation)
(require 'combobulate-rules)

(defgroup combobulate-fsharp nil
  "Configuration switches for F#."
  :group 'combobulate
  :prefix "combobulate-fsharp-")

(defun combobulate-fsharp-pretty-print-node-name (node default-name)
  "Pretty print the NODE name (falling back on DEFAULT-NAME) for F# mode."
  (let ((name (treesit-node-text node t)))
    (if (string-empty-p name)
        default-name
      (combobulate-string-truncate
       (replace-regexp-in-string (rx (| (>= 2 " ") "\n")) " " name) 40))))

(eval-and-compile
  (defconst combobulate-fsharp-definitions
    '((context-nodes
       '("identifier" "long_identifier" "long_identifier_or_op"
         "type_name" "const" "int" "float" "string" "bool" "char"
         "infix_op" "prefix_op" "op_identifier" "wildcard_pattern"))

      ;; F# is whitespace-sensitive, so nothing here may re-indent a
      ;; region on the user's behalf: the envelopes below carry their own
      ;; indentation explicitly with `save-column' rather than deferring
      ;; to the major mode's indentation engine.
      (navigate-down-into-lists nil)
      (indent-after-edit nil)
      (envelope-indent-region-function nil)
      (pretty-print-node-name-function #'combobulate-fsharp-pretty-print-node-name)
      (plausible-separators '(";" "," "|" "->" "|>"))

      (display-ignored-node-types
       '("let" "module" "namespace" "open" "type" "member" "and"
         "do" "val" "with" "then" "else" "rec" "mutable" "inline"))

      (envelope-procedure-shorthand-alist
       '((expressions . ((:activation-nodes
                          ((:nodes ((rule "_expression")))))))
         (module-elements . ((:activation-nodes
                              ((:nodes ((rule "_module_elem")))))))))

      ;; Every template here indents with `save-column' and literal
      ;; spaces rather than with `n>'.  `n>' would call the major mode's
      ;; indentation engine, and F# indentation is a guess even for the
      ;; mode that owns the buffer; `save-column' needs no guessing.  It
      ;; records the column on entry and restores it on exit, so the
      ;; idiom below -- wrap the head of a construct in `save-column' and
      ;; end its block with `n' -- puts point back at the construct's own
      ;; column, from which four literal spaces are one F# indent level.
      ;; Nest a second `save-column' around head and body together when a
      ;; template has to come back out to that column for an `else', a
      ;; `with', or another case.
      (envelope-list
       '((:description
          "let ... = ..."
          :key "l"
          :name "let-binding"
          :template ((save-column "let " (p name "Name") " =" n)
                     "    " @ r>))

         (:description
          "match ... with | ... -> ..."
          :key "m"
          :name "match-expression"
          :template ((save-column
                      (save-column "match " (p expr "Expression") " with" n)
                      (save-column "| " (p pat "Pattern") " ->" n)
                      "    " @ r> n)
                     (choice* :missing nil
                              :rest ((save-column "| " (p pat2 "Next Pattern") " ->" n)
                                     "    " @)
                              :name "add-case")))

         (:description
          "if ... then ... [else ...]"
          :key "i"
          :name "if-expression"
          :template ((save-column
                      (save-column "if " (p cond "Condition") " then" n)
                      "    " @ r> n)
                     (choice* :missing nil
                              :rest ((save-column "else" n) "    " @)
                              :name "else-branch")))

         (:description
          "try ... with | ... -> ..."
          :key "t"
          :name "try-with"
          :template ((save-column
                      (save-column "try" n)
                      "    " @ r> n)
                     (save-column "with" n)
                     (save-column "| " (p exn "Exception") " ->" n)
                     "    " @))

         (:description
          "for ... in ... do ..."
          :key "f"
          :name "for-loop"
          :template ((save-column "for " (p var "Variable") " in "
                                  (p seq "Sequence") " do" n)
                     "    " @ r>))

         (:description
          "while ... do ..."
          :key "w"
          :name "while-loop"
          :template ((save-column "while " (p cond "Condition") " do" n)
                     "    " @ r>))

         (:description
          "fun ... -> ..."
          :key "u"
          :name "fun-expression"
          :template ("fun " (p args "Arguments") " -> " @ r>))

         (:description
          "function | ... -> ..."
          :key "F"
          :name "function-expression"
          :template ((save-column
                      (save-column "function" n)
                      (save-column "| " (p pat "Pattern") " ->" n)
                      "    " @ r> n)
                     (choice* :missing nil
                              :rest ((save-column "| " (p pat2 "Next Pattern") " ->" n)
                                     "    " @)
                              :name "add-case")))

         (:description
          "module ... = ..."
          :key "M"
          :name "module-definition"
          :template ((save-column "module " (p name "Module Name") " =" n)
                     "    " @ r>))

         (:description
          "type ... = | ..."
          :key "U"
          :name "union-type"
          :template ((save-column
                      (save-column "type " (p name "Type Name") " =" n)
                      "    | " (p case "Case") @ n)
                     (choice* :missing nil
                              :rest ("    | " (p case2 "Next Case"))
                              :name "add-case")))

         (:description
          "type ... = { ... }"
          :key "R"
          :name "record-type"
          :template ((save-column "type " (p name "Type Name") " =" n)
                     "    { " (p field "Field") ": " (p ty "Type") @ " }"))

         (:description
          "type ...(...) = ..."
          :key "C"
          :name "class-type"
          :template ((save-column "type " (p name "Type Name") "("
                                  (p args "Constructor Args") ") =" n)
                     "    " @ r>))))

      (procedures-logical '((:activation-nodes ((:nodes (all))))))

      ;; The node set `M-h' and the cursor-editing commands expand
      ;; through.  It is everything by default, which is wrong here: the
      ;; two chain nodes each cover their own statement *and every
      ;; statement after it*, so expanding a selection walks through a
      ;; run of "this `let' plus the rest of the body" regions that do
      ;; not correspond to anything a reader would call a block.  Leaving
      ;; them out steps straight from the statement to the construct that
      ;; encloses it; nothing is lost, because a chain rung always starts
      ;; where its own statement does and that statement is still there.
      (procedures-default
       '((:activation-nodes
          ((:nodes (exclude (all)
                            "declaration_expression"
                            "sequential_expression"))))))

      ;; A "defun" in F# is whatever `C-M-a' and `C-M-e' should step
      ;; between: a binding, a type, a member, a nested module.
      ;; `named_module' and `namespace' are deliberately absent because
      ;; they span the whole file, which would make `C-M-a' bounce to the
      ;; top of the buffer instead of to the enclosing definition.
      ;;
      ;; So is `declaration_expression': a rung of the binding chain
      ;; reaches from its own `let' to the end of the body, so each rung
      ;; strictly contains the one below it, and a repeated `C-M-h' would
      ;; crawl up the `let's of the current block -- growing the region by
      ;; one binding at a time -- instead of reaching the enclosing
      ;; definition.  The `function_or_value_defn' inside every rung is
      ;; the binding itself and starts on the same `let', so nothing is
      ;; lost except `use' and `use!', which the grammar gives no node of
      ;; their own; those were never marked correctly anyway, since their
      ;; rung is the only node there and it runs to the end of the block.
      (procedures-defun
       '((:activation-nodes
          ((:nodes ("function_or_value_defn"
                    "value_declaration"
                    "member_defn"
                    "additional_constr_defn"
                    "type_definition"
                    "exception_definition"
                    "extern_binding"
                    "module_defn"))))))

      (procedures-sibling
       '(;; --- Chains of bindings ---------------------------------------
         ;;
         ;; `let a = 1' followed by `let b = 2' in a body is one
         ;; `declaration_expression' holding the other, so matching the
         ;; children of a single rung would only ever find the next one.
         ;; Rooting a recursive query at the *enclosing* rung and taking
         ;; the children of every rung below it flattens the tower back
         ;; into the run of statements it is written as.  Discarding the
         ;; rungs themselves leaves exactly the statements: each rung's
         ;; binding (a `function_or_value_defn', which starts on its
         ;; `let') plus the expression the innermost rung ends on.  The
         ;; discard also settles an ambiguity for editing commands, where
         ;; a rung and the binding inside it start at the same character
         ;; and only the binding is the statement a drag should move.
         ;;
         ;; The parent list is what keeps this off top-level bindings: a
         ;; chained rung hangs off another rung, off the binding it
         ;; belongs to, or off a member; a top-level `let' hangs off the
         ;; module, and so falls through to the module rule further down.
         (:activation-nodes
          ((:nodes ((rule "_expression"))
                   :position at
                   :has-parent ("declaration_expression"
                                "function_or_value_defn"
                                "method_or_prop_defn")))
          :selector (:choose parent
                     :match-query
                     (:query ((declaration_expression (_) @match))
                      :discard-rules ("declaration_expression")
                      :engine treesitter)))

         ;; --- Chains of statements --------------------------------------
         ;;
         ;; Same tower, built from `sequential_expression', and flattened
         ;; the same way.  This needs two passes: the first catches point
         ;; sitting on a nested rung (whose own parent is the rung above,
         ;; so that rooting the query at the parent sees the whole
         ;; chain), and the second catches the first statement, which has
         ;; no rung of its own.  They must
         ;; be separate procedures, and in this order, because for a
         ;; middle statement both the statement and the rung starting at
         ;; the same character match -- and only the rung has the parent
         ;; that can see backwards.
         (:activation-nodes
          ((:nodes ("sequential_expression")
                   :position at
                   :has-parent ("sequential_expression")))
          :selector (:choose parent
                     :match-query
                     (:query ((sequential_expression (_) @match))
                      :discard-rules ("sequential_expression")
                      :engine treesitter)))

         (:activation-nodes
          ((:nodes ((rule "_expression"))
                   :position at
                   :has-parent ("sequential_expression")))
          :selector (:choose parent
                     :match-query
                     (:query ((sequential_expression (_) @match))
                      :discard-rules ("sequential_expression")
                      :engine treesitter)))

         ;; --- Point on the delimiter that opens a group -----------------
         ;;
         ;; A group's own start is the `|' or `{' that opens it, and
         ;; `back-to-indentation' on such a line lands exactly there.
         ;; That position resolves to the group, whose members are then
         ;; the obvious siblings -- but the group is not a member of
         ;; anything, so without this the search continues up the tree
         ;; and finds the *enclosing* case instead, which for a nested
         ;; `match' means `C-M-n' leaps out of the inner block entirely.
         ;;
         ;; `:position at' is what keeps this from swallowing every
         ;; position inside the group; the same node types appear again
         ;; further down as an unrestricted fallback.
         ;;
         ;; `type_extension_elements' is deliberately absent.  It is a
         ;; group like the others, but in a class with a primary
         ;; constructor the grammar gives each member a wrapper of its
         ;; own, so matching the children of the one wrapper at point
         ;; yields a single member with no siblings.  The members rule
         ;; further down queries across every wrapper in the type and
         ;; gets the whole list; this rule would shadow it, because a
         ;; wrapper starts at the same character as the member it holds.
         (:activation-nodes
          ((:nodes ("rules" "union_type_cases" "enum_type_cases"
                    "record_fields" "union_type_fields"
                    "field_initializers"
                    "arguments_spec" "argument_patterns")
                   :position at))
          :selector (:choose node :match-children t))

         ;; --- Grouped constructs ----------------------------------------
         ;;
         ;; Everything the grammar does give us as a flat list of
         ;; children: match cases, union cases, record fields, list and
         ;; array elements, patterns, the branches of a conditional.
         (:activation-nodes
          ((:nodes ("rule") :has-parent ("rules"))
           (:nodes ("union_type_case") :has-parent ("union_type_cases"))
           (:nodes ("union_type_field") :has-parent ("union_type_fields"))
           (:nodes ("record_field") :has-parent ("record_fields"))
           (:nodes ("enum_type_case") :has-parent ("enum_type_cases"))
           (:nodes ("field_initializer") :has-parent ("field_initializers"))
           (:nodes ("extern_param") :has-parent ("extern_binding"))
           (:nodes ("argument_spec") :has-parent ("arguments_spec"))
           ;; `disjunct_pattern' (`| 1 | 2 ->'), `conjunct_pattern'
           ;; (`a & b') and `repeat_pattern' (`Some x, Some y') are
           ;; deliberately absent.  Those three start where their first
           ;; child does, so in a match case they start where the `rule'
           ;; does too -- and because a procedure tries the nodes at
           ;; point before their parents, listing them here would make
           ;; `C-M-n' step between the alternatives of one case's
           ;; pattern instead of between the cases, which is what the
           ;; block looks like it is a list of.  The bracketed patterns
           ;; below have no such clash: their children start after the
           ;; opening delimiter, so the case start still resolves to the
           ;; case and stepping into `[x; y]' still walks the elements.
           (:nodes ((rule "_pattern"))
                   :has-parent ("argument_patterns"
                                "list_pattern" "array_pattern"
                                "record_pattern" "paren_pattern"))
           ;; `elif_expression' is not an `_expression' in this grammar,
           ;; so it has to be named outright or `C-M-n' stops at the
           ;; `if' and never reaches the `elif' or `else' branch.
           (:nodes ((rule "_expression") "elif_expression")
                   :position at
                   :has-parent ("list_expression" "array_expression"
                                "paren_expression" "begin_end_expression"
                                "brace_expression" "ce_expression"
                                "infix_expression" "if_expression"
                                "elif_expression" "for_expression"
                                "while_expression" "try_expression"))
           (:nodes ((rule "_type"))
                   :has-parent ("compound_type" "function_type"
                                "type_arguments" "constrained_type")))
          :selector (:match-children t))

         ;; Tuples nest to the right and applications to the left, so a
         ;; recursive query is what makes either read back as the flat
         ;; list it looks like in the buffer.
         (:activation-nodes
          ((:nodes ((rule "_expression"))
                   :position at
                   :has-parent ("tuple_expression")))
          :selector (:choose parent
                     :match-query
                     (:query ((tuple_expression (_) @match))
                      :discard-rules ("tuple_expression")
                      :engine treesitter)))

         (:activation-nodes
          ((:nodes ((rule "_expression"))
                   :position at
                   :has-parent ("application_expression")))
          :selector (:choose parent
                     :match-query
                     (:query ((application_expression (_) @match))
                      :discard-rules ("application_expression")
                      :engine treesitter)))

         ;; --- Members of a type -----------------------------------------
         ;;
         ;; One `type_extension_elements' per member in a class with a
         ;; primary constructor, one for all of them in a record or a
         ;; `type X with' extension: querying the children of every
         ;; wrapper under the type flattens both into one list of members.
         ;; `:has-ancestor' is what puts the type itself -- rather than
         ;; one wrapper -- at the root of that query.
         (:activation-nodes
          ((:nodes ("member_defn" "function_or_value_defn"
                    "interface_implementation" "additional_constr_defn"
                    "value_declaration" "do_expression"
                    "application_expression")
                   :position at
                   :has-ancestor ("anon_type_defn" "record_type_defn"
                                  "union_type_defn" "enum_type_defn"
                                  "interface_type_defn" "type_extension"
                                  "delegate_type_defn")))
          :selector (:choose parent
                     :match-query
                     (:query ((type_extension_elements (_) @match))
                      :engine treesitter)))

         ;; Members of an `interface ... with' block, or of an object
         ;; expression, are plain children of the block.
         (:activation-nodes
          ((:nodes ("member_defn")
                   :has-parent ("interface_implementation" "object_expression")))
          :selector (:match-children t))

         ;; --- Landing on a container rather than on a member -------------
         ;;
         ;; A line that starts with `|' -- a union case, a match case --
         ;; puts point on an anonymous token that belongs to the *group*
         ;; rather than to the case that follows it, so the search for a
         ;; navigable node walks past the case and settles on the group.
         ;; The same happens on a record's `{' and on the `else' of a
         ;; conditional.  Rather than let those positions fall through to
         ;; the module rule below -- which would leap to the next
         ;; top-level definition -- these rules step from the group into
         ;; its members.
         ;;
         ;; They must come before the module rule and after everything
         ;; specific: a group is an ancestor of every one of its members,
         ;; so at any position inside one this rule can match.
         (:activation-nodes
          ((:nodes ("union_type_cases" "enum_type_cases" "record_fields"
                    "union_type_fields" "rules" "field_initializers"
                    "arguments_spec" "argument_patterns"
                    "type_extension_elements" "if_expression"
                    "elif_expression" "match_expression" "try_expression"
                    "function_expression" "list_expression"
                    "array_expression")))
          :selector (:choose node :match-children t))

         (:activation-nodes
          ((:nodes ((rule "_type_defn_body"))))
          :selector (:choose node
                     :match-children (:discard-rules ("type_name" "attributes"))))

         ;; --- Definitions in a module ------------------------------------
         ;;
         ;; The fallback for definitions: wherever point is inside a
         ;; definition, and nothing more specific applies, `C-M-n' should
         ;; reach the next definition.  The module's own name is a child
         ;; like any other, so it is discarded -- stepping back from the
         ;; first definition in a file should not land on the module
         ;; header.
         (:activation-nodes
          ((:nodes ((rule "_module_elem"))
                   :has-parent ("file" "named_module" "namespace"
                                "module_defn")))
          :selector (:match-children
                     (:discard-rules ("long_identifier" "identifier"
                                      "attributes" "access_modifier"))))

         ;; The first definition in a module needs its own rule for the
         ;; same reason as the groups above: point at its start resolves
         ;; to the module rather than to the definition, so without this
         ;; the first `open' or `let' in a file cannot be stepped away
         ;; from.
         (:activation-nodes
          ((:nodes ("named_module" "namespace" "module_defn" "file")))
          :selector (:choose node
                     :match-children
                     (:discard-rules ("long_identifier" "identifier"
                                      "attributes" "access_modifier"))))))

      ;; Neither chain node appears anywhere below, and that is what makes
      ;; `C-M-u' work.  Going up asks for the nearest enclosing node of a
      ;; type these procedures mention, and a rung of the binding chain is
      ;; the tree parent of the rung beneath it -- so listing them would
      ;; make `C-M-u' climb the run of `let's one line at a time instead of
      ;; reaching the definition that opens the block.  Nothing is lost
      ;; going down: a rung starts on the same `let' as the
      ;; `function_or_value_defn' inside it, which is listed, and that
      ;; binding's `body:' is where a descent should land anyway.
      (procedures-hierarchy
       '(;; Descend from a binding to the thing it binds, skipping the
         ;; name and the argument patterns.  The body is a grandchild
         ;; (declaration_expression -> function_or_value_defn -> body:),
         ;; so a recursive query reaches it in one step where a child
         ;; match would stop on the binding's name.
         (:activation-nodes
          ((:nodes ("value_declaration" "function_or_value_defn")
                   :position at))
          :selector (:choose node
                     :match-query
                     (:query ((function_or_value_defn body: (_) @match))
                      :engine treesitter)))

         ;; From a type definition, descend straight to the first case,
         ;; field or member.  The type's body node starts on the type's
         ;; *name*, so a plain child match would spend a keypress landing
         ;; on the name; this query skips over it to the contents.
         ;;
         ;; A primary constructor's arguments are not among them, even
         ;; though they are the first thing inside the type: they sit on
         ;; the same line as the name, so descending onto them looks like
         ;; nothing happened, and it costs a keypress on the way to the
         ;; body every time.  They stay reachable from the type's name --
         ;; point on `P' in `type P(x: int) =' is on `anon_type_defn',
         ;; whose rule further down matches all of its children.
         (:activation-nodes
          ((:nodes ("type_definition") :position at))
          :selector (:choose node
                     :match-query
                     (:query ([(union_type_cases (union_type_case) @match)
                               (record_fields (record_field) @match)
                               (enum_type_cases (enum_type_case) @match)
                               (type_extension_elements (_) @match)])
                      :engine treesitter)))

         ;; A member's body hangs off `method_or_prop_defn'; descend to
         ;; the body rather than to the member's name.  Both rules come
         ;; before the group rule below, which would otherwise match the
         ;; `type_extension_elements' wrapping a member -- a wrapper that
         ;; starts at the same character as the member it holds, so
         ;; descending into it moves nowhere.
         (:activation-nodes
          ((:nodes ("member_defn") :position at))
          :selector (:choose node
                     :match-children (:match-rules ("method_or_prop_defn"
                                                    "member_signature"
                                                    "value_declaration"
                                                    "additional_constr_defn"))))

         (:activation-nodes
          ((:nodes ("method_or_prop_defn") :position at))
          :selector (:choose node
                     :match-query
                     (:query ((method_or_prop_defn body: (_) @match))
                      :engine treesitter)))

         (:activation-nodes
          ((:nodes ("union_type_defn" "record_type_defn" "enum_type_defn"
                    "anon_type_defn" "interface_type_defn" "type_extension"
                    "type_abbrev_defn" "delegate_type_defn")
                   :position at))
          :selector (:choose node
                     :match-children (:discard-rules ("type_name"))))

         (:activation-nodes
          ((:nodes ("union_type_cases" "record_fields" "enum_type_cases"
                    "union_type_fields" "type_extension_elements"
                    "field_initializers" "rules" "arguments_spec"
                    "primary_constr_args" "argument_patterns")
                   :position at))
          :selector (:choose node :match-children t))

         (:activation-nodes
          ((:nodes ("if_expression" "elif_expression" "match_expression"
                    "try_expression" "function_expression" "rule"
                    "union_type_case" "record_field" "enum_type_case"
                    "union_type_field" "field_initializer")
                   :position at))
          :selector (:choose node :match-children t))

         (:activation-nodes
          ((:nodes ("named_module" "namespace" "module_defn") :position at))
          :selector (:choose node
                     :match-children
                     (:discard-rules ("long_identifier" "identifier"
                                      "attributes" "access_modifier"))))

         (:activation-nodes
          ((:nodes ("list_expression" "array_expression" "tuple_expression"
                    "paren_expression" "begin_end_expression" "brace_expression"
                    "anon_record_expression" "ce_expression"
                    "application_expression" "infix_expression"
                    "prefixed_expression" "typed_expression"
                    "typecast_expression" "dot_expression" "index_expression"
                    "mutate_expression" "do_expression" "fun_expression"
                    "for_expression" "while_expression" "object_expression"
                    "interface_implementation" "exception_definition"
                    "paren_pattern" "identifier_pattern" "typed_pattern"
                    "as_pattern" "cons_pattern" "list_pattern" "array_pattern"
                    "record_pattern" "attribute" "attributes" "compound_type"
                    "function_type" "generic_type" "paren_type")
                   :position at))
          :selector (:choose node :match-children t))

         (:activation-nodes
          ((:nodes ("file")))
          :selector (:choose node :match-children t)))))))

(defun combobulate-fsharp-setup (_)
  "Setup function for F# mode with Combobulate."
  (setq-local combobulate-navigate-down-into-lists nil))

(define-combobulate-language
 :name fsharp
 :major-modes (fsharp-mode fsharp-ts-mode)
 :custom combobulate-fsharp-definitions
 :setup-fn combobulate-fsharp-setup)

(provide 'combobulate-fsharp)
;;; combobulate-fsharp.el ends here
