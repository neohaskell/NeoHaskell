import { describe, it, expect, vi } from 'vitest'
import { render, screen } from '../../test/render'
import userEvent from '@testing-library/user-event'
import { FieldsEditor } from './FieldsEditor'
import type { Field } from '../../model/types'

describe('FieldsEditor', () => {
  it('fields_editor_add_edit_remove', async () => {
    const user = userEvent.setup()
    const fields: Field[] = [{ name: 'orderId', type: 'UUID' }]
    const onChange = vi.fn()
    const { rerender } = render(<FieldsEditor fields={fields} onChange={onChange} />)

    // Add a field → appended with defaults.
    await user.click(screen.getByTestId('add-field'))
    expect(onChange).toHaveBeenLastCalledWith([
      { name: 'orderId', type: 'UUID' },
      { name: 'field', type: 'String' },
    ])

    // Edit the type of the existing field.
    onChange.mockClear()
    const typeInputs = screen.getAllByLabelText('field type')
    await user.type(typeInputs[0], 'X')
    expect(onChange).toHaveBeenLastCalledWith([{ name: 'orderId', type: 'UUIDX' }])

    // Remove the field.
    onChange.mockClear()
    await user.click(screen.getByLabelText('remove field'))
    expect(onChange).toHaveBeenLastCalledWith([])

    // Renders rows for each field.
    rerender(<FieldsEditor fields={[{ name: 'a', type: 'B' }, { name: 'c', type: 'D' }]} onChange={onChange} />)
    expect(screen.getAllByLabelText('field name')).toHaveLength(2)
  })
})
