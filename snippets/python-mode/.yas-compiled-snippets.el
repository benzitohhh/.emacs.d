;;; Compiled snippets and support files for `python-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'python-mode
                     '(("deb" "pdb.set_trace()\n" "pdb.set_trace()" nil nil nil nil nil nil)
                       ("imp" "import pdb\n" "import pdb" nil nil nil nil nil nil)
                       ("impt" "import unittest\n" "import unittest" nil nil nil nil nil nil)
                       ("test" "class Test${1:ClassName}(unittest.TestCase):\n    \"\"\"$2\n    \"\"\"\n\n    def setUp(self):\n        pass\n\n    def test_${3:TestName}(self):\n        self.assertEqual(1, 1)\n        self.assertTrue(True)\n        $0\n\nunittest.main()\n" "unittest class" nil nil
                        ((yas/indent-line 'fixed))
                        nil nil nil)))


;;; Do not edit! File generated at Wed Oct 15 19:14:08 2014
