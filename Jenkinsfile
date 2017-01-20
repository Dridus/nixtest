node {
  stage('Build') {
    sh 'stack clean build'
  }

  stage('Test') {
    sh 'stack test'
  }
}

