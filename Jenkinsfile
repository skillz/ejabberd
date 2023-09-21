@Library(['gitops-pipeline-library@v4']) _

switch(env.BRANCH_NAME) {
  case ~/PR-[0-9]+/:
    testPipeline()
    buildPipeline()
    deploymentPipeline(['gitops-qa'], true)
    break
  case 'development':
    testPipeline()
    buildPipeline()
    deploymentPipeline(['gitops-qa'], true)
    break
  case 'master':
    testPipeline()
    buildPipeline()
    deploymentPipeline(['gitops-staging'], false)
    deploymentPipeline(['gitops-production'], false)
    break
}

def pullModules() {
  def moduleRepositories = [
    'mod_push_skillz',
    'mod_beam_stats'
  ]

  moduleRepositories.each { repo ->
    git.cloneRepo(
      destination: ".ejabberd_modules/sources/${repo}",
      org: 'skillz',
      repo: repo
    )
  }

  git.cloneRepo(
    destination: ".ejabberd_modules/sources/mod_pottymouth",
    org: 'skillz',
    repo: 'mod_pottymouth',
    branch: 'feature-filter-ticketz-tier-id'
  )
}

def testPipeline() {
  stage('Test') {
    echo "TODO enable a way to run just the tests before building and deploying the image."
    echo "Currently tests run as part of the docker build."
  }
}

def buildPipeline() {
  stage('Image') {
    workerTemplates.github {
      workerTemplates.docker {
        node(POD_LABEL) {
          def scmVars = checkout(scm)
          def imageTag = scmVars.GIT_COMMIT

          container('github') {
            pullModules()
          }

          container('docker') {
            dockerBuildPush(
              tag: imageTag,
              onlyArtifactory: true
            )
          }
        }
      }
    }
  }
}

def deploymentPipeline(List repos, boolean autoMerge = false) {
  stage('GitOps Deploy') {
    workerTemplates.github {
      node(POD_LABEL) {
        def scmVars = checkout(scm)
        def imageTag = scmVars.GIT_COMMIT

        container('github') {
          def prBranch = "${repoName()}@${scmVars.GIT_BRANCH}"
          def modifyFile = "apps/${repoName()}/release.yaml"
          createPR(
            branch: prBranch,
            waitForStatus: false,
            repos: repos,
            autoMerge: autoMerge,
            modifyYaml: [
              file        : modifyFile,
              key         : "imageTag",
              desiredValue: imageTag
            ]
          )
        }
      }
    }
  }
}
