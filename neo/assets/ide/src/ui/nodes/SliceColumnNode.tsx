import { EditableLabel } from './EditableLabel'
import classes from './SliceColumnNode.module.css'

interface Props {
  data: {
    label: string
    chapterName?: string | null
    highlighted?: boolean
    flashing?: boolean
    onRename?: (name: string) => void
    onSelect?: () => void
  }
}

export function SliceColumnNodeComponent({ data }: Props) {
  const columnCls = [
    classes.column,
    data.highlighted ? classes.columnHighlighted : '',
    data.flashing ? 'em-flash' : '',
  ].join(' ')
  const headerCls = [
    classes.header,
    data.highlighted ? classes.headerHighlighted : '',
    data.flashing ? 'em-flash' : '',
  ].join(' ')
  const labelCls = [
    classes.label,
    data.highlighted ? classes.labelHighlighted : '',
  ].join(' ')
  return (
    <div className={columnCls}>
      <div className={`${headerCls} em-pointer-auto`} onClick={data.onSelect}>
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
