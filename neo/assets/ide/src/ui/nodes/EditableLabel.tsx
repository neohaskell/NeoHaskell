import { useState, useRef, useEffect, useCallback } from 'react'
import classes from './EditableLabel.module.css'

interface EditableLabelProps {
  label: string
  onRename: (newName: string) => void
}

export function EditableLabel({ label, onRename }: EditableLabelProps) {
  const [editing, setEditing] = useState(false)
  const [value, setValue] = useState(label)
  const inputRef = useRef<HTMLInputElement>(null)

  useEffect(() => {
    if (editing) {
      inputRef.current?.select()
    }
  }, [editing])

  const commit = useCallback(() => {
    setEditing(false)
    if (value !== label && value.trim() !== '') {
      onRename(value.trim())
    } else {
      setValue(label)
    }
  }, [value, label, onRename])

  const handleKeyDown = useCallback(
    (e: React.KeyboardEvent) => {
      if (e.key === 'Enter') {
        e.preventDefault()
        commit()
      } else if (e.key === 'Escape') {
        e.preventDefault()
        setEditing(false)
        setValue(label)
      }
    },
    [commit, label],
  )

  if (editing) {
    return (
      <input
        ref={inputRef}
        className={classes.input}
        value={value}
        onChange={(e) => setValue(e.target.value)}
        onBlur={commit}
        onKeyDown={handleKeyDown}
      />
    )
  }

  return (
    <span
      onDoubleClick={(e) => {
        e.stopPropagation()
        setValue(label)
        setEditing(true)
      }}
      className={classes.label}
    >
      {label}
    </span>
  )
}
