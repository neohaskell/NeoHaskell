import { EditableLabel } from './EditableLabel'
import classes from './EntityLaneNode.module.css'

interface Props {
  data: {
    label: string
    highlighted?: boolean
    flashing?: boolean
    onRename?: (name: string) => void
    onSelect?: () => void
  }
}

export function EntityLaneNodeComponent({ data }: Props) {
  const laneCls = [
    classes.lane,
    data.highlighted ? classes.laneHighlighted : '',
    data.flashing ? 'em-flash' : '',
  ].join(' ')
  const labelCls = [
    classes.label,
    data.highlighted ? classes.labelHighlighted : '',
  ].join(' ')
  return (
    <div className={laneCls}>
      {/* 100px clickable strip before the slices start. */}
      <div className={`${classes.labelCol} em-pointer-auto`} onClick={data.onSelect}>
        <span className={labelCls}>
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
