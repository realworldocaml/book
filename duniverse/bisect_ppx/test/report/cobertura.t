  $ echo "(lang dune 2.7)" > dune-project
  $ cat > dune <<'EOF'
  > (executable
  >  (name test)
  >  (instrumentation (backend bisect_ppx)))
  > EOF
  $ dune exec ./test.exe --instrument-with bisect_ppx
  $ bisect-ppx-report cobertura coverage.xml --verbose
  Info: found *.coverage files in './'
  $ cat coverage.xml
  <?xml version="1.0" ?>
  <coverage lines-valid="6" lines-covered="2" line-rate="0.333333">
    <sources>
      <source>.</source>
    </sources>
    <package name="." line-rate="0.333333">
      <classes>
        <class name="test.ml" filename="test.ml" line-rate="0.333333">
          <lines>
            <line number="2" hits="1"/>
            <line number="5" hits="0"/>
            <line number="8" hits="1"/>
            <line number="13" hits="0"/>
            <line number="14" hits="0"/>
            <line number="16" hits="0"/>
          </lines>
        </class>
      </classes>
    </package>
  </coverage>
