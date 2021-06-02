---
title: How Text-Clone Works
subtitle: A Quick Developer's Guide
author: nobiot
created: 2021-06-01
modified: 2021-06-01T231921
---

# What is this Document?

Text-Clone is a small Emacs package that lets you synchronize regions of text across multiple buffers. It is an "engine" that drives the live-sync feature of Org-transclusion. I call it an engine because it contains no interaction functions for users to manually call.

I have written this document with Elisp programmers in mind as its main reader. In it, I will try to help you understand how the whole thing works. A barebone test Elisp script is also available as a companion to this document so that you can test drive it in your own Emacs. In the source of Text-Clone, I have also tried to detail how each variable and function is meant to work. With all these together, I am hoping that you can craft your own application with using Text-Clone or further extend it.

# Credits

Text-Clone is an extention of text-clone functions written as part of GNU Emacs in subr.el.  The first adaption to extend text-clone functions to work across buffers was published in StackExchange by the user named Tobias in March 2020. It can be found at [this thread in StackExchange](https://emacs.stackexchange.com/questions/56201/is-there-an-emacs-package-which-can-mirror-a-region/56202#56202). 

I have made further adaptations for version 0.0.1 in order for it to work with the Org-transclusion package.

# Text-Clone: `text-clone.el` 

It is written based on two built-in functions, `text-clone-create` and `text-clone--maintain` but is not using them -- no dependency on them.

It's a small package. It incldues two variables and 5 functions, and each of them comes with a docstring explananing what it is meant to do. I have made available a set of an Elips script and test text files that le you test-drive Text-Clone.

Out of the five functions, you only need to deal with 3, and one of them is a just a convenience wrapper function to document and standardize the parameteres to pass to the built-in `make-overlay` function. Effectively, you will need to consciously use `text-clone-set-overlays` and `text-clone-delete-overlays` functions. As a developer, you should be aware of what the two variable and the rest of the functions are meant to do but you don't need to write your own functions to use them in order to use the "live-sync" feature.

- Function: `text-clone-make-overlay (beg end &optional buf)`
   A wrapper function. I recommend to use it to create overlays for text-clone but not mandatory. 
   It is meant to document and standardize the parameters passed to the built-in `make-overlay` function.
   
- Function: `text-clone-set-overlays (&rest overlays)`
   Call it explicitly in your function; pass overlays to required properties for live-sync. It also sets `post-command-hook` locally with `text-clone-command-h` function, and the global variable `text-clone-overlays ` to start tracking the overlays.

- Function:  text-clone-delete-overlays ()
  Call it explicitly in your function. It deletes the Text-Clone overlays tracked by the variable `text-clone-overlays `.

- Function: text-clone-post-command-h ()
  You don't need to call it explicitly. Set and unset by `text-clone-overlays` and `text-clone-delete-opverlay` respectively. 

- Function: text-clone-live-sync
  You don't need to call it explicitly. Set by `text-clone-set-overlays` to appropriate overlay properties for live-sync

# Test Script and Text Files

The script is a bare minimum simple test script. It should come with three test text files -- two of them are empty. Simply evaluate the whole buffer, and call `test-text-clone` command. It will copy the text from `test-clone-original.txt`, and copy them over to the two empty files, `test-clone-1.txt` and `test-clone-2.txt`, and adds faces to overlays.  You can test the live-sync feature from any of the three text files.

Call `test-text-clone-finish` to end the testing. It will empty both the clone text files, leaving `txt` as it is. It does not reset itself; you can have any text as you like in it.
