(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



open OUnit2
open Test_helpers

let test name arguments env =
  test name begin fun () ->
    run "git init > /dev/null";
    run
      ("export GIT_COMMITTER_NAME=C && export GIT_COMMITTER_EMAIL=d && " ^
      "export GIT_COMMITTER_DATE='Jan 1 00:00:00 2020 +0000' && " ^
      "git commit --allow-empty --author 'A <b>' -m Foo " ^
      "--date 'Jan 1 00:00:00 2020 +0000' > /dev/null");
    compile (with_bisect ()) "fixtures/send-to/source.ml";
    run "./a.out -inf 0 -sup 3 > /dev/null";
    run "./a.out -inf 7 -sup 11 > /dev/null";
    run "echo '/*' > output";
    report ~env ("send-to --dry-run " ^  arguments) ~r:">> output 2>&1 || true";
    run "echo '*/' >> output";
    run "cat *.json >> output 2> /dev/null || true";
    diff (Printf.sprintf "fixtures/send-to/%s.reference.json" name)
  end

let none = []

let travis_no_job_id = [
  "TRAVIS", "true"
]

let travis = [
  "TRAVIS", "true";
  "TRAVIS_JOB_ID", "42";
]

let travis_with_repo_token = [
  "TRAVIS", "true";
  "TRAVIS_JOB_ID", "42";
  "COVERALLS_REPO_TOKEN", "abcxyz";
]

let circleci_no_repo_token = [
  "CIRCLECI", "true";
  "CIRCLE_BUILD_NUM", "43";
]

let circleci = [
  "CIRCLECI", "true";
  "CIRCLE_BUILD_NUM", "43";
  "COVERALLS_REPO_TOKEN", "abcxyz";
]

let circleci_pr = [
  "CIRCLECI", "true";
  "CIRCLE_BUILD_NUM", "43";
  "CIRCLE_PULL_REQUEST", "2";
  "COVERALLS_REPO_TOKEN", "abcxyz";
]

let circleci_repo_token_precedence = [
  "CIRCLECI", "true";
  "CIRCLE_BUILD_NUM", "43";
  "COVERALLS_REPO_TOKEN", "xyzabc";
  "COVERAGE_REPO_TOKEN", "abcxyz";
]

let github_actions_no_repo_token = [
  "GITHUB_ACTIONS", "true";
  "GITHUB_RUN_NUMBER", "43";
]

let github_actions = [
  "GITHUB_ACTIONS", "true";
  "GITHUB_RUN_NUMBER", "43";
  "COVERALLS_REPO_TOKEN", "abcxyz";
]

let github_actions_pr = [
  "GITHUB_ACTIONS", "true";
  "GITHUB_RUN_NUMBER", "43";
  "COVERALLS_REPO_TOKEN", "abcxyz";
  "PULL_REQUEST_NUMBER", "2";
]

let tests = "send-to" >::: [
  test "bad-service"
    "bad-service" none;

  test "case-sensitivity"
    "coveralls" none;

  test "no-ci"
    "Coveralls" none;

  test "no-job-id"
    "Coveralls" travis_no_job_id;

  test "travis-coveralls"
    "Coveralls" travis;

  test "travis-codecov"
    "Codecov" travis;

  test "overrides-basic"
    "Coveralls --service-name foo --service-job-id bar" travis;

  test "extras"
    "Coveralls --service-number 1 --service-pull-request 2 --parallel" travis;

  test "git"
    "Coveralls --git" travis;

  test "travis-repo-token"
    "Coveralls --repo-token abcxyz" travis;

  test "travis-repo-token-ignore"
    "Coveralls" travis_with_repo_token;

  test "circleci-no-repo-token"
    "Coveralls" circleci_no_repo_token;

  test "circleci-coveralls"
    "Coveralls" circleci;

  test "circleci-repo-token-precedence"
    "Coveralls" circleci_repo_token_precedence;

  test "circleci-repo-token-argument"
    "Coveralls --repo-token defuvw" circleci_no_repo_token;

  test "circleci-repo-token-override"
    "Coveralls --repo-token uvwdef" circleci;

  test "circleci-pr-option"
    "Coveralls --service-pull-request 1" circleci;

  test "circleci-pr"
    "Coveralls" circleci_pr;

  test "circleci-pr-override"
    "Coveralls --service-pull-request 3" circleci_pr;

  test "circleci-codecov"
    "Codecov" circleci_no_repo_token;

  test "github-actions-no-repo-token"
    "Coveralls" github_actions_no_repo_token;

  test "github-actions-coveralls"
    "Coveralls" github_actions;

  test "github-actions-repo-token-argument"
    "Coveralls --repo-token defuvw" github_actions_no_repo_token;

  test "github-actions-repo-token-override"
    "Coveralls --repo-token uvwdef" github_actions;

  test "github-actions-pr-option"
    "Coveralls --service-pull-request 1" github_actions;

  test "github-actions-pr"
    "Coveralls" github_actions_pr;

  test "github-actions-pr-override"
    "Coveralls --service-pull-request 3" github_actions_pr;

  test "github-actions-codecov"
    "Codecov" github_actions_no_repo_token;
]
