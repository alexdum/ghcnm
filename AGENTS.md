<RULE[huggingface_deployment]>
# Hugging Face Spaces Deployment (Storage Limit Bypass)
**Context:** Pushing full Git history containing large files to Hugging Face Spaces quickly exhausts their strict 1GB Git LFS storage limit.
**Rule:** When automating deployments to Hugging Face Spaces via GitHub Actions (especially for data-heavy apps), **never push the full Git history**. Instead, create an orphaned branch to force-push a single, history-less snapshot.
*Implementation pattern:*
```bash
git checkout --orphan deploy
git add -A
git commit -m "Deploy snapshot"
git push hf deploy:main --force
```
</RULE[huggingface_deployment]>

<RULE[github_actions_empty_dirs]>
# GitHub Actions & Git-Ignored Directories
**Context:** Python scripts crashing in CI/CD pipelines because they try to write to a folder that isn't checked into Git (like `misc/data`).
**Rule:** When writing GitHub Actions workflows that execute scripts requiring local data/output directories, check if those directories are in `.gitignore`. If they are, **always explicitly create them** (e.g., `mkdir -p <dir>`) in the workflow before executing the script, as the checkout action will not create them.
</RULE[github_actions_empty_dirs]>

<RULE[plotly_annotation_positioning]>
# Plotly Annotation Positioning
**Context:** Text annotations placed below a Plotly graph disappearing when the graph is maximized due to relative scaling.
**Rule:** When positioning text annotations below a Plotly chart, **avoid using negative relative `y` coordinates** (e.g., `y = -0.12`). Instead, anchor the annotation to the bottom of the plot area (`y = 0`) and use a fixed pixel shift (`yshift = -20`), ensuring you also increase the bottom margin (`margin = list(b = 30)`). This prevents the text from being pushed off-canvas when the plot expands on large screens.
</RULE[plotly_annotation_positioning]>
