  $ echo "(lang dune 2.7)" > dune-project
  $ cat > dune <<'EOF'
  > (executable
  >  (name test)
  >  (instrumentation (backend bisect_ppx)))
  > EOF
  $ dune exec ./test.exe --instrument-with bisect_ppx

  $ git init --quiet
  $ export GIT_COMMITTER_NAME=C
  $ export GIT_COMMITTER_EMAIL=d
  $ export GIT_COMMITTER_DATE='Jan 1 00:00:00 2020 +0000'
  $ git commit --allow-empty --author 'A <b>' -m Foo --date 'Jan 1 00:00:00 2020 +0000' --quiet


From Travis to Coveralls.

  $ bisect-ppx-report send-to --dry-run No-such-service
  Error: send-to: unknown coverage service 'No-such-service'
  [1]

  $ bisect-ppx-report send-to --dry-run coveralls
  Error: send-to: unknown coverage service 'coveralls'
  [1]

  $ bisect-ppx-report send-to --dry-run Coveralls
  Info: will write coverage report to 'coverage.json'
  Error: unknown CI service or not in CI
  [1]

  $ TRAVIS=true bisect-ppx-report send-to --dry-run Coveralls
  Info: will write coverage report to 'coverage.json'
  Info: detected CI: Travis
  Info: using service name 'travis-ci'
  Info: using job ID variable $TRAVIS_JOB_ID
  Error: expected job id in $TRAVIS_JOB_ID
  [1]

  $ TRAVIS=true TRAVIS_JOB_ID=100 bisect-ppx-report send-to --dry-run Coveralls
  Info: will write coverage report to 'coverage.json'
  Info: detected CI: Travis
  Info: using service name 'travis-ci'
  Info: using job ID variable $TRAVIS_JOB_ID
  Info: found coverage files in './'
  Info: sending to Coveralls with command:
  Info: curl -L -F json_file=@./coverage.json https://coveralls.io/api/v1/jobs
  $ cat coverage.json
  {
      "service_name": "travis-ci",
      "service_job_id": "100",
  
  
      "source_files": [
          {
              "name": "test.ml",
              "source_digest": "55ec0167abb99a3141afb092c92d0e23",
              "coverage": [null,1,null,null,0,null,null,1]
          }
      ]
  }


From Travis to Codecov.

  $ TRAVIS=true TRAVIS_JOB_ID=100 bisect-ppx-report send-to --dry-run Codecov
  Info: will write coverage report to 'coverage.json'
  Info: detected CI: Travis
  Info: using service name 'travis-ci'
  Info: using job ID variable $TRAVIS_JOB_ID
  Info: found coverage files in './'
  Info: sending to Codecov with command:
  Info: curl -s https://codecov.io/bash | bash -s -- -Z -f coverage.json
  $ cat coverage.json
  {
      "service_name": "travis-ci",
      "service_job_id": "100",
  
  
      "source_files": [
          {
              "name": "test.ml",
              "source_digest": "55ec0167abb99a3141afb092c92d0e23",
              "coverage": [null,1,null,null,0,null,null,1]
          }
      ]
  }


From CircleCI to Coveralls.

  $ CIRCLECI=true bisect-ppx-report send-to --dry-run Coveralls
  Info: will write coverage report to 'coverage.json'
  Info: detected CI: CircleCI
  Info: using service name 'circleci'
  Info: using job ID variable $CIRCLE_BUILD_NUM
  Error: expected job id in $CIRCLE_BUILD_NUM
  [1]

  $ CIRCLECI=true CIRCLE_BUILD_NUM=100 bisect-ppx-report send-to --dry-run Coveralls
  Info: will write coverage report to 'coverage.json'
  Info: detected CI: CircleCI
  Info: using service name 'circleci'
  Info: using job ID variable $CIRCLE_BUILD_NUM
  Info: $CIRCLE_PULL_REQUEST not set
  Error: expected repo token in $COVERALLS_REPO_TOKEN
  [1]

  $ CIRCLECI=true CIRCLE_BUILD_NUM=100 CIRCLE_PULL_REQUEST=10 bisect-ppx-report send-to --dry-run Coveralls
  Info: will write coverage report to 'coverage.json'
  Info: detected CI: CircleCI
  Info: using service name 'circleci'
  Info: using job ID variable $CIRCLE_BUILD_NUM
  Info: using PR number variable $CIRCLE_PULL_REQUEST
  Error: expected repo token in $COVERALLS_REPO_TOKEN
  [1]

  $ CIRCLECI=true CIRCLE_BUILD_NUM=100 CIRCLE_PULL_REQUEST=10 COVERALLS_REPO_TOKEN=abc bisect-ppx-report send-to --dry-run Coveralls
  Info: will write coverage report to 'coverage.json'
  Info: detected CI: CircleCI
  Info: using service name 'circleci'
  Info: using job ID variable $CIRCLE_BUILD_NUM
  Info: using PR number variable $CIRCLE_PULL_REQUEST
  Info: using repo token variable $COVERALLS_REPO_TOKEN
  Info: including git info
  Info: found coverage files in './'
  Info: sending to Coveralls with command:
  Info: curl -L -F json_file=@./coverage.json https://coveralls.io/api/v1/jobs
  $ cat coverage.json
  {
      "service_name": "circleci",
      "service_job_id": "100",
      "service_pull_request": "10",
      "repo_token": "abc",
      "git":{"head":{"id":"5689966cc697646c10975ff9355863bc12744ea0","author_name":"A","author_email":"b","committer_name":"C","committer_email":"d","message":"Foo"},"branch":"master","remotes":{}},
  
      "source_files": [
          {
              "name": "test.ml",
              "source_digest": "55ec0167abb99a3141afb092c92d0e23",
              "coverage": [null,1,null,null,0,null,null,1]
          }
      ]
  }


From CircleCI to Codecov.

  $ CIRCLECI=true CIRCLE_BUILD_NUM=100 bisect-ppx-report send-to --dry-run Codecov
  Info: will write coverage report to 'coverage.json'
  Info: detected CI: CircleCI
  Info: using service name 'circleci'
  Info: using job ID variable $CIRCLE_BUILD_NUM
  Info: found coverage files in './'
  Info: sending to Codecov with command:
  Info: curl -s https://codecov.io/bash | bash -s -- -Z -f coverage.json
  $ cat coverage.json
  {
      "service_name": "circleci",
      "service_job_id": "100",
  
  
      "source_files": [
          {
              "name": "test.ml",
              "source_digest": "55ec0167abb99a3141afb092c92d0e23",
              "coverage": [null,1,null,null,0,null,null,1]
          }
      ]
  }


From GitHub Actions to Coveralls.

  $ GITHUB_ACTIONS=true bisect-ppx-report send-to --dry-run Coveralls
  Info: will write coverage report to 'coverage.json'
  Info: detected CI: GitHub Actions
  Info: using service name 'github'
  Info: using job ID variable $GITHUB_RUN_NUMBER
  Error: expected job id in $GITHUB_RUN_NUMBER
  [1]

  $ GITHUB_ACTIONS=true GITHUB_RUN_NUMBER=100 bisect-ppx-report send-to --dry-run Coveralls
  Info: will write coverage report to 'coverage.json'
  Info: detected CI: GitHub Actions
  Info: using service name 'github'
  Info: using job ID variable $GITHUB_RUN_NUMBER
  Info: $PULL_REQUEST_NUMBER not set
  Error: expected repo token in $COVERALLS_REPO_TOKEN
  [1]

  $ GITHUB_ACTIONS=true GITHUB_RUN_NUMBER=100 COVERALLS_REPO_TOKEN=abc bisect-ppx-report send-to --dry-run Coveralls
  Info: will write coverage report to 'coverage.json'
  Info: detected CI: GitHub Actions
  Info: using service name 'github'
  Info: using job ID variable $GITHUB_RUN_NUMBER
  Info: $PULL_REQUEST_NUMBER not set
  Info: using repo token variable $COVERALLS_REPO_TOKEN
  Info: including git info
  Info: found coverage files in './'
  Info: sending to Coveralls with command:
  Info: curl -L -F json_file=@./coverage.json https://coveralls.io/api/v1/jobs
  $ cat coverage.json
  {
      "service_name": "github",
      "service_job_id": "100",
      "repo_token": "abc",
      "git":{"head":{"id":"5689966cc697646c10975ff9355863bc12744ea0","author_name":"A","author_email":"b","committer_name":"C","committer_email":"d","message":"Foo"},"branch":"master","remotes":{}},
  
      "source_files": [
          {
              "name": "test.ml",
              "source_digest": "55ec0167abb99a3141afb092c92d0e23",
              "coverage": [null,1,null,null,0,null,null,1]
          }
      ]
  }


From GitHub Actions to Codecov

  $ GITHUB_ACTIONS=true GITHUB_RUN_NUMBER=100 bisect-ppx-report send-to --dry-run Codecov
  Info: will write coverage report to 'coverage.json'
  Info: detected CI: GitHub Actions
  Info: using service name 'github'
  Info: using job ID variable $GITHUB_RUN_NUMBER
  Info: found coverage files in './'
  Info: sending to Codecov with command:
  Info: curl -s https://codecov.io/bash | bash -s -- -Z -f coverage.json
  $ cat coverage.json
  {
      "service_name": "github",
      "service_job_id": "100",
  
  
      "source_files": [
          {
              "name": "test.ml",
              "source_digest": "55ec0167abb99a3141afb092c92d0e23",
              "coverage": [null,1,null,null,0,null,null,1]
          }
      ]
  }
