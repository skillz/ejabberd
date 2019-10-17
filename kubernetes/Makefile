EXTERNAL_CONFIGMAP=tf-ejabberd-external
EXTERNAL_CONFIGMAP_NS=server
KUSTOMIZE_VARIANT=base

## ------------------------- Kustomize Targets --------------------------------
external-config: # Download external config map for reference in kustomize
	kubectl get configmap $(EXTERNAL_CONFIGMAP) -n $(EXTERNAL_CONFIGMAP_NS) -o yaml > $(KUSTOMIZE_VARIANT)/external-tf-map.yaml

kustomize: external-config # See the generated manifests for an environment
	kustomize build $(KUSTOMIZE_VARIANT)

kapply: external-config # Apply the generated manifests for an environment to the current kube context
	kustomize build $(KUSTOMIZE_VARIANT) | kubectl apply -f -

destroy: cloned-chart external-config # Destroys all ejabberd kubernetes resources, warning does not destroy external TF resources
	kustomize build $(KUSTOMIZE_VARIANT) | kubectl delete -f -
