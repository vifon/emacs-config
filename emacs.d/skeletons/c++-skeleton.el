(define-skeleton c++-skeleton
  "" ""
  "// File: "(file-name-nondirectory (buffer-file-name))"\n"
  (let ((header (concat (file-name-nondirectory (file-name-sans-extension (buffer-file-name))) ".hpp")))
    (if (file-exists-p header)
        (concat "\n#include \"" header "\"\n\n"
                (when (progn
                        (setq v1 (skeleton-read "Namespace: "))
                        (not (string= "" v1)))
                  (concat "namespace " v1 " {\n\n")))
        (concat "\n#include <iostream>\n"
                "#include <string>\n\n"

                "int main(int argc, char *argv[])\n"
                "{\n    ")))
    _
    (if (not (file-exists-p (concat (file-name-nondirectory (file-name-sans-extension (buffer-file-name))) ".hpp")))
        (concat "\n    return 0;\n"
                "}\n")
        (when (not (string= "" v1))
          (concat "\n\n} // namespace " v1))))


(define-skeleton c-skeleton
  "" ""
  "/* File: "(file-name-nondirectory (buffer-file-name))" */\n"
  (let ((header (concat (file-name-nondirectory (file-name-sans-extension (buffer-file-name))) ".h")))
    (if (file-exists-p header)
        (concat "\n#include \"" header "\"\n\n")
        (concat "\n#include <stdio.h>\n"
                "#include <stdlib.h>\n\n"

                "int main(int argc, char *argv[])\n"
                "{\n    ")))
  _
    (if (not (file-exists-p (concat (file-name-nondirectory (file-name-sans-extension (buffer-file-name))) ".h")))
        (concat "\n    return 0;\n"
                "}\n")))

(define-skeleton c++-header-skeleton
  "" ""
  "// File: "(file-name-nondirectory (buffer-file-name))"\n"
  (and (setq v1 (skeleton-read "Namespace: ")) nil)
  (and (setq v2 (upcase
             (concat
              (unless (string= "" v1)
                (concat v1 "_"))
              (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))))
       nil)
  "#ifndef _h_" v2 "_\n"
  "#define _h_" v2 "_\n\n"
  (when (not (string= "" v1))
    (concat "namespace " v1 " {\n\n"))
  _
  (when (not (string= "" v1))
    (concat "\n\n} // namespace " v1))
  "\n\n"
  "#endif\n"
  )

(define-skeleton c-header-skeleton
  "" ""
  "/* File: "(file-name-nondirectory (buffer-file-name))" */\n"
  "#ifndef _h_" (upcase (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))) "_\n"
  "#define _h_" (upcase (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))) "_\n\n"
  _
  "\n\n"
  "#endif\n"
  )

(define-skeleton c++-qt-skeleton
  "" ""
  "// File: "(file-name-nondirectory (buffer-file-name))"\n"
  "\n#include <QApplication>\n\n"

  "int main(int argc, char *argv[])\n"
  "{\n"
  "    QApplication app(argc, argv);\n\n"
  "    " _ "\n\n"
  "    return app.exec();\n"
  "}\n"
  )

(define-skeleton c-ncurses-skeleton
  "" ""
  "// File: "(file-name-nondirectory (buffer-file-name))"\n"
  "\n#include <ncurses.h>\n\n"

  "int main(int argc, char *argv[])\n"
  "{\n"
  "    initscr();\n"
  "    cbreak();\n"
  "    noecho();\n"
  "    start_color();\n"
  "    keypad(stdscr, 1);\n\n    "

  _
  "\n\n    return endwin();\n"
  "}\n"
  )

(define-skeleton c++-unittest-skeleton
  "" ""
  "// File: "(file-name-nondirectory (buffer-file-name))"\n"
  "#ifndef _h_" (upcase (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))) "_\n"
  "#define _h_" (upcase (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))) "_\n\n"
  "#include <cppunit/TestFixture.h>\n"
  "#include <cppunit/extensions/HelperMacros.h>\n\n"

  "class "(capitalize (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))" : public CPPUNIT_NS::TestFixture\n"
  "{\n"
  "    CPPUNIT_TEST_SUITE("(capitalize (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))");\n"
  "    CPPUNIT_TEST_SUITE_END();\n"
  "  public:\n"
  "    void setUp();\n"
  "    void tearDown();\n"
  "  protected:\n"
  "  private:\n"
  "};\n"
  "\n\n"
  "#endif\n"
  )

(define-skeleton c++-unittest-main-skeleton
  "" ""
  "// File: "(file-name-nondirectory (buffer-file-name))"\n"
  "\n#include <cppunit/CompilerOutputter.h>\n"
  "#include <cppunit/extensions/TestFactoryRegistry.h>\n"
  "#include <cppunit/TestResult.h>\n"
  "#include <cppunit/TestResultCollector.h>\n"
  "#include <cppunit/TestRunner.h>\n"
  "#include <cppunit/BriefTestProgressListener.h>\n\n"

  "int main(int argc, char* argv[])\n"
  "{\n"
  "    CPPUNIT_NS::TestResult testresult;\n"
  "    CPPUNIT_NS::TestResultCollector collectedresults;\n"
  "    testresult.addListener(&collectedresults);\n"
  "    CPPUNIT_NS::BriefTestProgressListener progress;\n"
  "    testresult.addListener(&progress);\n"
  "    CPPUNIT_NS::TestRunner testrunner;\n"
  "    testrunner.addTest(CPPUNIT_NS::TestFactoryRegistry::getRegistry().makeTest());\n"
  "    testrunner.run(testresult);\n"
  "    CPPUNIT_NS::CompilerOutputter compileroutputter(&collectedresults, std::cerr);\n"
  "    compileroutputter.write();\n"
  "    return collectedresults.wasSuccessful() ? 0 : 1;\n"
  "}\n"
  )


(define-auto-insert "\\.\\(C\\|cc\\|cpp\\)$" 'c++-skeleton)
(define-auto-insert "\\.c$" 'c-skeleton)
(define-auto-insert "\\.\\(H\\|hpp\\)$" 'c++-header-skeleton)
(define-auto-insert "\\.h$" 'c-header-skeleton)
