import { EditableLabel } from './EditableLabel'
import classes from './SubmodelBandNode.module.css'

interface Props {
  data: {
    label: string
    onRename?: (name: string) => void
  }
}

// A submodel band is a translucent full-bleed rectangle drawn BEHIND the graph
// (lowest z-index) that visually contains a feature's chapters/slices. Its body
// is pointer-transparent so nodes inside stay interactive; only the title chip
// (which floats just above the frame, like a tab) captures clicks for rename.
// Deletion lives in the Features sidebar, not on the frame.
export function SubmodelBandNodeComponent({ data }: Props) {
  return (
    <div className={classes.band}>
      <div className={classes.chip}>
        <span className={classes.label}>
          {data.onRename ? (
            <EditableLabel label={data.label} onRename={data.onRename} />
          ) : (
            data.label
          )}
        </span>
      </div>
    </div>
  )
}
