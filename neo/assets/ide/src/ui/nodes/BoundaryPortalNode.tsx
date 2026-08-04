import { NodeHandles } from './NodeHandles'
import classes from './BoundaryPortalNode.module.css'

interface Props {
  data: {
    label: string
    featureName: string
    direction: 'in' | 'out'
    onNavigate?: () => void
  }
}

// On-screen stub for the off-feature endpoint of a cross-feature edge. Keeps the
// edge resolvable (React Flow drops edges with a missing endpoint), names the
// node + feature it bridges to, and is clickable to jump there. Carries the full
// 4-side handle set so the portal edge renders regardless of routing side.
export function BoundaryPortalNodeComponent({ data }: Props) {
  return (
    <div
      data-testid="boundary-portal"
      onClick={data.onNavigate}
      title={`Go to feature: ${data.featureName}`}
      className={classes.portal}
    >
      <NodeHandles />
      <div className={classes.label}>{data.label}</div>
      <div className={classes.meta}>
        {data.direction === 'out' ? 'to ' : 'from '}
        {data.featureName}
      </div>
    </div>
  )
}
