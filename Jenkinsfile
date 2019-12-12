pipeline {
    options {
        timestamps()
            buildDiscarder(logRotator(artifactDaysToKeepStr: '20', artifactNumToKeepStr: '20', daysToKeepStr: '20', numToKeepStr: '20'))
    }

    environment {
        VERSION = "0.4.${BUILD_NUMBER}"
    }

    parameters {
        string(name: 'AWS_ACCOUNT_ID', defaultValue: env.AWS_ACCOUNT_ID) // Get this from AWS
        string(name: 'ECR_REGION', defaultValue: 'us-west-2')
        string(name: 'EJABBERD_BRANCH', defaultValue: 'development')
        string(name: 'MODULES_BRANCH', defaultValue: 'development')
        string(name: 'SKILLZ_ENVIRONMENT', defaultValue: env.SKILLZ_ENVIRONMENT)
    }

    agent {
        label "${params.SKILLZ_ENVIRONMENT}"
    }

    stages {
        stage ('Cleanup') {
            steps {
                cleanWs(cleanWhenAborted: true, cleanWhenFailure: true, cleanWhenSuccess: true, cleanWhenUnstable: true, deleteDirs: true)
            }
        }

        stage ('Clone') {
            steps {
                // Clone ejabberd
                dir('ejabberd') {
                    checkout([$class: 'GitSCM',
                                branches: [[name: params.EJABBERD_BRANCH]],
                                doGenerateSubmoduleConfigurations: false,
                                poll: false,
                                extensions: [],
                                submoduleCfg: [],
                                userRemoteConfigs: [[url: "git@github.com:skillz/ejabberd"]]])
                }

                // Clone private modules
                // The credentials ID here are unique to jenkins-ci-01 skillz jenkins server
                // Must be adjusted if moving this job to a different jenkins
                dir('ejabberd/.ejabberd_modules/sources/mod_push_skillz') {
                    checkout([$class: 'GitSCM',
                                branches: [[name: params.MODULES_BRANCH]],
                                doGenerateSubmoduleConfigurations: false,
                                poll: false,
                                extensions: [],
                                submoduleCfg: [],
                                userRemoteConfigs: [[credentialsId: '3c0610c5-1718-4483-b24e-83b799ab8629',
                                                    url: "git@github.com:skillz/mod_push_skillz"]]])
                }
                dir('ejabberd/.ejabberd_modules/sources/mod_pottymouth') {
                    // Check out master, there isn't a development branch
                    checkout([$class: 'GitSCM',
                                branches: [[name: (params.MODULES_BRANCH == 'master' || params.MODULES_BRANCH == 'development' ? 'master' : params.MODULES_BRANCH)]],
                                doGenerateSubmoduleConfigurations: false,
                                poll: false,
                                extensions: [],
                                submoduleCfg: [],
                                userRemoteConfigs: [[credentialsId: '3c0610c5-1718-4483-b24e-83b799ab8629',
                                                    url: "git@github.com:skillz/mod_pottymouth"]]])
                }
                dir('ejabberd/.ejabberd_modules/sources/mod_beam_stats') {
                    checkout([$class: 'GitSCM',
                                branches: [[name: params.MODULES_BRANCH]],
                                doGenerateSubmoduleConfigurations: false,
                                poll: false,
                                extensions: [],
                                submoduleCfg: [],
                                userRemoteConfigs: [[credentialsId: '3c0610c5-1718-4483-b24e-83b799ab8629',
                                                    url: "git@github.com:skillz/mod_beam_stats"]]])
                }
            }
        }
        /*
         Currently, we build the QA version of ejabberd from a development branch and the staging/production version from the master branch
         As such, we are only promoting a single artifact once from staging to production and we are building more often in QA.
         The image versions between QA vs Staging/Production will not match with this strategy.
        */
        stage ('Docker') {
            steps {
                dir('ejabberd') {
                    script {
                        sh """
                        docker build -t ejabberd:latest .

                        docker tag ejabberd:latest docker.dev.skillz.com:5000/ejabberd:latest
                        docker tag ejabberd:latest docker.dev.skillz.com:5000/ejabberd:${VERSION}
                        docker tag ejabberd:latest ${params.AWS_ACCOUNT_ID}.dkr.ecr.${params.ECR_REGION}.amazonaws.com/ejabberd:latest
                        docker tag ejabberd:latest ${params.AWS_ACCOUNT_ID}.dkr.ecr.${params.ECR_REGION}.amazonaws.com/ejabberd:${params.SKILLZ_ENVIRONMENT}
                        docker tag ejabberd:latest ${params.AWS_ACCOUNT_ID}.dkr.ecr.${params.ECR_REGION}.amazonaws.com/ejabberd:${VERSION}

                        docker push docker.dev.skillz.com:5000/ejabberd:latest
                        docker push docker.dev.skillz.com:5000/ejabberd:${VERSION}
                        docker push ${params.AWS_ACCOUNT_ID}.dkr.ecr.${params.ECR_REGION}.amazonaws.com/ejabberd:latest
                        docker push ${params.AWS_ACCOUNT_ID}.dkr.ecr.${params.ECR_REGION}.amazonaws.com/ejabberd:${params.SKILLZ_ENVIRONMENT}
                        docker push ${params.AWS_ACCOUNT_ID}.dkr.ecr.${params.ECR_REGION}.amazonaws.com/ejabberd:${VERSION}

                        docker rmi ejabberd:latest
                        docker rmi docker.dev.skillz.com:5000/ejabberd:latest
                        docker rmi docker.dev.skillz.com:5000/ejabberd:${VERSION}
                        docker rmi ${params.AWS_ACCOUNT_ID}.dkr.ecr.${params.ECR_REGION}.amazonaws.com/ejabberd:latest
                        docker rmi ${params.AWS_ACCOUNT_ID}.dkr.ecr.${params.ECR_REGION}.amazonaws.com/ejabberd:${params.SKILLZ_ENVIRONMENT}
                        docker rmi ${params.AWS_ACCOUNT_ID}.dkr.ecr.${params.ECR_REGION}.amazonaws.com/ejabberd:${VERSION}
                        """
                    }
                }
            }
        }
    }

    post {
        success {
            slackSend channel: "#${params.SKILLZ_ENVIRONMENT}", color: 'good', message: "Ejabberd Docker Pipeline ${env.BUILD_NUMBER} Build Success!"

            //  Clean up the workspace
            deleteDir()
        }
        failure {
            slackSend channel: "#${params.SKILLZ_ENVIRONMENT}", color: 'danger', message: "Ejabberd Docker Pipeline ${env.BUILD_NUMBER} Build Failed!"
        }
    }
}
